-module(watts_plugin).
%%
%% Copyright 2016 - 2017 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").
-behaviour(gen_server).
-include("watts.hrl").

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([
         get_cred_list/1,
         get_count/2,
         exists/2,
         request/4,
         revoke/2,
         get_params/1
        ]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% -spec get_list(binary()) -> {ok, [watts:cred()]}.
get_cred_list(UserInfo) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    get_credential_list(UserId).

%% -spec get_count(binary(), binary()) -> {ok, non_neg_integer()}.
get_count(UserInfo, ServiceId) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    get_credential_count(UserId, ServiceId).

%% -spec exists(binary(), binary()) -> true | false.
exists(UserInfo, CredId) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    case get_credential(UserId, CredId) of
        {ok, _} -> true;
        _ -> false
    end.

%% -spec request(binary(), binary(), watts:user_info(), binary(), map()|atom(),
%%               list()) -> {ok, map(), list()} | {error, any(), list()}.
request(ServiceId, UserInfo, Interface, Params) ->
    {ok, Limit} = watts_service:get_credential_limit(ServiceId),
    {ok, QueueName} = watts_service:get_queue(ServiceId),
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    {ok, Count} = get_credential_count(UserId, ServiceId),
    Enabled = watts_service:is_enabled(ServiceId),
    Allowed = watts_service:is_allowed(UserInfo, ServiceId),
    ParamsValid = watts_service:are_params_valid(Params, ServiceId),
    case { Allowed, Enabled, ParamsValid, Count < Limit } of
        {true, true, true, true} ->
            {ok, Pid} = watts_plugin_sup:new_worker(),
            Config = #{ action => request,
                        queue => QueueName,
                        service_id => ServiceId,
                        user_info => UserInfo,
                        interface => Interface,
                        params => Params},
            Result = watts_plugin_runner:request_action(Config, Pid),
            handle_result(Result, Config);
        {false, _, _, _} ->
            {error, user_not_allowed};
        {true, false, _, _} ->
            {error, service_disabled};
        {true, true, false, _} ->
            {error, invalid_params};
        {true, true, true, false} ->
            {error, limit_reached}
    end.

%% -spec revoke(binary(), binary(), map()) ->
%%     {ok, watts:cred(), list()} | {error, any(), list()}.
revoke(CredentialId, UserInfo) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    CredentialInfo = get_credential(UserId, CredentialId),
    revoke_credential(CredentialInfo, UserInfo).


revoke_credential({ok, #{ service_id := ServiceId, cred_state := _CredState,
                          cred_id := _CredId } = Credential}, UserInfo) ->
    ServiceExists = watts_service:exists(ServiceId),
    {ok, QueueName} = watts_service:get_queue(ServiceId),
    Config = maps:merge(#{action => revoke,
                          queue => QueueName,
                          user_info => UserInfo
                         }, Credential),
    revoke_or_drop(ServiceExists, Config);
revoke_credential({error, Reason}, _UserInfo) ->
    {error, Reason}.


revoke_or_drop(true, Config) ->
    {ok, Pid} = watts_plugin_sup:new_worker(),
    Result=watts_plugin_runner:request_action(Config, Pid),
    handle_result(Result, Config);
revoke_or_drop(false, #{service_id := ServiceId,
                        user_info := UserInfo,
                        cred_id := CredId} ) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    DropEnabled = ?CONFIG(allow_dropping_credentials),
    UMsg = "the service does not exist, please contact the administrator",
    case DropEnabled of
        true ->
            lager:warning("service ~p: dropping revocation request for ~p",
                          [ServiceId, CredId]),
            remove_credential(UserId, CredId);
        _ ->
            Msg = "service ~p does not exist, revocation impossible",
            LogMsg = io_lib:format(Msg, [ServiceId]),
            return(error, #{user_msg => UMsg, log_msg => LogMsg})
    end.



get_params(ServiceId) ->
    {ok, Pid} = watts_plugin_sup:new_worker(),
    Config = #{ action => parameter,
                service_id => ServiceId},
    %% Result = watts_plugin_runner:get_params(ServiceId, Pid),
    Result = watts_plugin_runner:request_action(Config, Pid),
    handle_result(Result, Config).


handle_result({ok, #{result := Result}=Map, Log}, Info) ->
    AResult = result_to_atom(Result),
    handle_result(AResult, Map, Log, Info);
handle_result({ok, _Map, _Log}, #{service_id := ServiceId} ) ->
    LogMsg = io_lib:format("plugin missing 'result': service ~p", [ServiceId]),
    UMsg = "the plugin had an error, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg});
handle_result({error, Map, Log}, Info) ->
    LogMsg = io_lib:format("plugin error: ~p ~p ~p", [Map, Info, Log]),
    UMsg = "the plugin had an error, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg=>LogMsg}).


%% REQUEST handling
handle_result(ok, #{credential := Cred0, state := CredState}, _Log,
              #{action := request} = Info) ->
    %% a valid credential response
    UserInfo = maps:get(user_info, Info),
    Interface = maps:get(interface, Info),
    ServiceId = maps:get(service_id, Info),
    Cred = validate_credential_values(Cred0),
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    case sync_store_credential(UserId, ServiceId, Interface, CredState) of
        {ok, CredId} ->
            return(result, #{id => CredId, entries => Cred});
        Error ->
            return(error, {storing, Error})
    end;
handle_result(ok, Map, Log, #{action := request}) ->
    %% a bad credential response
    LogMsg = io_lib:format("bad response to a request: ~p [~p]", [Map, Log]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg});
handle_result(oidc_login, #{provider := ProviderId, msg := Msg}, _Log,
              #{action := request} = _Info) ->
    %% an OpenId Connect login request
    return(oidc_login, #{provider => ProviderId, msg => Msg});
handle_result(oidc_login, Map, Log, #{action := request}) ->
    %% a bad OpenID Connect login request
    LogMsg = io_lib:format("bad response to a request: ~p [~p]", [Map, Log]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg});
handle_result(error, #{user_msg := UMsg}=Map, _Log, #{ action := request} ) ->
    %% a valid error response
    LogMsg = log_msg(Map, UMsg),
    return(error, #{user_msg => UMsg, log_msg => LogMsg});
handle_result(error, Map, _Log, #{ action := request})->
    %% a bad error response
    LogMsg = io_lib:format("bad error response: ~p", [Map]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg});


%% REVOKE handling
handle_result(ok, _, _Log, #{action := revoke} = Info) ->
    %% a valid credential response
    UserInfo = maps:get(user_info, Info),
    CredId = maps:get(cred_id, Info),
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    remove_credential(UserId, CredId);
handle_result(error, #{user_msg := UMsg}=Map, _Log, #{ action := revoke} ) ->
    %% a valid error response
    LogMsg = log_msg(Map, UMsg),
    return(error, #{user_msg => UMsg, log_msg => LogMsg});
handle_result(error, Map, Log, #{ action := revoke})->
    %% a bad error response
    LogMsg = io_lib:format("bad error response: ~p [~p]", [Map, Log]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg});

%% PARAMETER handling
handle_result(ok,
              #{conf_params := _, request_params := _, version := _} = Result,
              _Log, #{ action := parameter } ) ->
    %% valid parameter response
    return(result, maps:with([conf_params, request_params, version], Result));
handle_result(ok, Result, _Log, #{ action := parameter } ) ->
    %% invalid parameter response
    NeededKeys = [conf_params, request_params, version, result],
    Keys = maps:keys(Result),
    MissingKeys = lists:subtract(NeededKeys, Keys),
    LogMsg = io_lib:format("bad parameter response: ~p missing keys ~p",
                           [Result, MissingKeys]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{log_msg => LogMsg, user_msg => UMsg});
handle_result(error, Result, Log, #{ action := parameter } ) ->
    %% invalid parameter response
    LogMsg = io_lib:format("bad parameter response: ~p [~p]", [Result, Log]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{log_msg => LogMsg, user_msg => UMsg});


%% EVERYTHING ELSE => ERROR
handle_result(Result, Map, _Log, Info) ->
    Action = maps:get(action, Info),
    ServiceId = maps:get(service_id, Info),
    LogMsg = io_lib:format("plugin for service ~p bad response: ~p - ~p [~p]",
                           [ServiceId, Action, Result, Map]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{user_msg => UMsg, log_msg => LogMsg}).


log_msg(Map, UMsg) ->
    maps:get(log_msg, Map, io_lib:format("error response: ~p", [UMsg])).



return(result, Result) ->
    {ok, Result};
return(oidc_login, Result) ->
    {oidc_login, Result};
return(error, Data) ->
    {error, Data}.

result_to_atom(Result)
  when is_atom(Result) ->
    Result;
result_to_atom(Result) ->
    try binary_to_existing_atom(Result, utf8) of
        Atom ->
            Atom
    catch _:_ ->
            unknown
    end.




sync_store_credential(UserId, ServiceId, Interface, CredState) ->
    gen_server:call(?MODULE, {store_credential, UserId, ServiceId, Interface,
                              CredState}).

-record(state, {
         }).


init([]) ->
    {ok, #state{}}.

handle_call({store_credential, UserId, ServiceId, Interface, CredState}, _From,
            State) ->
    Result = store_credential(UserId, ServiceId, Interface,
                                          CredState),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_credential(UserId, CredentialId) ->
    case get_credential(CredentialId) of
        {ok, Cred} ->
            case maps:get(user_id, Cred, undefined) of
                UserId -> {ok, Cred};
                _ -> {error, bad_user}
            end;
        Other -> Other
    end.

% functions with data access
get_credential_count(UserId, ServiceId) ->
    watts_persistent:credential_service_count(UserId, ServiceId).

get_credential(CredId) ->
    watts_persistent:credential_fetch(CredId).

get_credential_list(UserId) ->
    watts_persistent:credential_fetch_list(UserId).

store_credential(UserId, ServiceId, Interface, CredentialState) ->
    SameStateAllowed = watts_service:allows_same_state(ServiceId),
    watts_persistent:credential_store(UserId, ServiceId, Interface,
                                   CredentialState, SameStateAllowed).

remove_credential(UserId, CredentialId) ->
    case watts_persistent:credential_delete(UserId, CredentialId) of
        ok ->
            return(result, #{});
        Error ->
            return(error, {deleting, Error})
    end.


validate_credential_values(Credential) ->
    validate_credential_values(Credential, []).

validate_credential_values([], ValidatedCredential) ->
    lists:reverse(ValidatedCredential);
validate_credential_values([#{type:=<<"text">>, value:=Val0}=Entry|T],
                           ValCred) ->
    Value = value_to_binary(Val0),
    NewEntry = maps:put(value, Value, Entry),
    validate_credential_values(T, [ NewEntry | ValCred]);
validate_credential_values([#{type:=<<"textarea">>, value:=Val0}=Entry|T],
                           ValCred) ->
    Value = value_to_binary(Val0),
    NewEntry = maps:put(value, Value, Entry),
    validate_credential_values(T, [ NewEntry | ValCred]);
validate_credential_values([#{type:=<<"textfile">>, value:=Val0}=Entry|T],
                           ValCred) ->
    Value = value_to_file(Val0),
    NewEntry = maps:put(value, Value, Entry),
    validate_credential_values(T, [ NewEntry | ValCred]);
validate_credential_values([#{type:=Type}=Entry|T], ValCred) ->
    lager:critical("unknown type ~p in credential, skipping Entry ~p",
                   [Type, Entry]),
    validate_credential_values(T, ValCred);
validate_credential_values([Entry|T], ValCred) ->
    lager:critical("no type in Entry ~p *SKIPPING*", [Entry]),
    validate_credential_values(T, ValCred).


value_to_binary(Val) when is_binary(Val)->
    Val;
value_to_binary(Val0) ->
    list_to_binary(io_lib:format("~p", [Val0])).

value_to_file(Val) when is_binary(Val)->
    Val;
value_to_file(Val) when is_list(Val)->
    file_lines_to_binary(Val, <<>>).

file_lines_to_binary([], File) ->
    File;
file_lines_to_binary([H | T], File) ->
    NewLine = value_to_binary(H),
    file_lines_to_binary(T, << File/binary, NewLine/binary >>).
