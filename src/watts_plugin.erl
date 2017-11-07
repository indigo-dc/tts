%% @doc this module implements the high level view of a plugin and service.
%% It exports the high level functions like
%% <ul>
%% <li> request </li>
%% <li> revoke </li>
%% </ul>
%% So triggering a plugin runner for a given service and validating the result
%% afterwards.
%% This module is not aware of the actual run binary.

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

-export_type([result/0]).

-type cred_entry() :: #{ name => binary(),
                         type => binary(),
                         valye => binary(),
                         _ => _
                       }.

-type result() :: {ok, #{id => binary(), entries => [cred_entry()] }}|
                  {ok, #{version => binary(),
                         request_params => [any()],
                         conf_params => [any()] }}|
                  {oidc_login, map()}|
                  {error, atom() | tuple() | map()}.

-type config() :: #{action =>  parameter | request | revoke ,
                    service_id => binary(),
                    queue => atom(),
                    user_id => undefined | binary(),
                    user_info => watts_userinfo:userinfo() | undefined,
                    cred_state => binary() | undefined,
                    params => map() | undefined,
                    cred_id => binary() | undefined,
                    _ => _
                   }.


%% @doc start the gen_server process linked to the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noparams, []).


%% @doc stop the process
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% @doc get the list of all credentials for the given user.
-spec get_cred_list(watts_userinfo:info()) -> {ok, [watts:credential()]}.
get_cred_list(UserInfo) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    get_credential_list(UserId).

%% @doc return the number of credentials of a user for a specific service.
-spec get_count(watts_userinfo:info(), binary()) -> {ok, non_neg_integer()}.
get_count(UserInfo, ServiceId) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    get_credential_count(UserId, ServiceId).

%% @doc checks if the credential for the given user exists.
-spec exists(watts_userinfo:info(), binary()) -> true | false.
exists(UserInfo, CredId) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    case get_credential(UserId, CredId) of
        {ok, _} -> true;
        _ -> false
    end.

%% @doc perform a request at the service for the user.
-spec request(ServiceId :: binary(), UserInfo :: watts_userinfo:userinfo(),
              Interface :: binary(), Params:: map()) -> result().
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
            Config = config(#{ action => request,
                               queue => QueueName,
                               service_id => ServiceId,
                               user_info => UserInfo,
                               interface => Interface,
                               params => Params}),
            Result = run_plugin(Config),
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

%% @doc try to revoke the given credential for the user
-spec revoke(binary(), watts_userinfo:info()) -> result().
revoke(CredentialId, UserInfo) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    CredentialInfo = get_credential(UserId, CredentialId),
    revoke_credential(CredentialInfo, UserInfo).


%% @doc revoke the credential for user, if found.
-spec revoke_credential( MaybeCredential, watts_userinfo:info())
                       -> result()
                              when MaybeCredential:: {ok, watts:credential()} |
                                                     {error, atom()}.
revoke_credential({ok, #{ service_id := ServiceId, cred_state := _CredState,
                          cred_id := _CredId } = Credential}, UserInfo) ->
    ServiceExists = watts_service:exists(ServiceId),
    {ok, QueueName} = watts_service:get_queue(ServiceId),
    Config = maps:merge(#{action => revoke,
                          queue => QueueName,
                          user_info => UserInfo
                         }, Credential),
    revoke_or_drop(ServiceExists, config(Config));
revoke_credential({error, Reason}, _UserInfo) ->
    {error, Reason}.

%% @doc perform the revocation or maybe drop the credential.
-spec revoke_or_drop(boolean(), config()) -> result().
revoke_or_drop(true, Config) ->
    Result = run_plugin(Config),
    handle_result(Result, Config);
revoke_or_drop(false, Config) ->
    DropEnabled = ?CONFIG(allow_dropping_credentials, false),
    maybe_drop(DropEnabled, Config).


%% @doc drop the credential if allowed, else error.
-spec maybe_drop(boolean(), config()) -> result().
maybe_drop(true, #{service_id := ServiceId, user_info := UserInfo,
                   cred_id := CredId} ) ->
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    lager:warning("service ~p: dropping revocation request for ~p",
                  [ServiceId, CredId]),
    remove_credential(UserId, CredId);
maybe_drop(false, #{service_id := ServiceId, cred_id := CredId} ) ->
    UMsg = "the service does not exist, please contact the administrator",
    Msg = "service ~p does not exist, revocation of ~p impossible",
    LogMsg = io_lib:format(Msg, [ServiceId, CredId]),
    return(error, #{user_msg => UMsg, log_msg => LogMsg}).


%% @doc perform the 'parameter' request at the plugin for the service.
-spec get_params(binary()) -> result().
get_params(ServiceId) ->
    Config = config(#{ action => parameter,
                       service_id => ServiceId}),
    Result = run_plugin(Config),
    handle_result(Result, Config).

%% @doc handle the results or a plugin run, including validation
%% @todo: send email in case of plugin errors #401
-spec handle_result(watts_plugin_runner:result(), config()) -> result().
handle_result({ok, #{result := Result}=Map, Output}, Info) ->
    AResult = result_to_atom(Result),
    handle_result(AResult, Map, Output, Info);
handle_result({ok, Map, Output}, Info) ->
    handle_result(undefined, Map, Output, Info);
handle_result({error, Reason, Output}, Info) ->
    handle_result(plugin_error, Reason, Output, Info).


%% @doc handle the results or a plugin run, including validation
%% @todo: send email in case of plugin errors #401
-spec handle_result( Result, MapOrReason, Output, Info) -> result()
    when Result :: ok | error | oidc_login | undefined | plugin_error | any(),
         MapOrReason :: map() | atom() | tuple(),
         Output :: watts_plugin_runner:output(),
         Info :: config().
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
handle_result(oidc_login, #{provider := ProviderId, msg := Msg}, _Log,
              #{action := request} = _Info) ->
    %% an OpenId Connect login request
    return(oidc_login, #{provider => ProviderId, msg => Msg});
handle_result(error, #{user_msg := UMsg}=Map, _Log, #{ action := request} ) ->
    %% a valid error response
    LogMsg = log_msg(Map, UMsg),
    return(error, #{user_msg => UMsg, log_msg => LogMsg});


%% REVOKE handling
handle_result(ok, _, _Log, #{action := revoke} = Info) ->
    %% a valid revoke response
    UserInfo = maps:get(user_info, Info),
    CredId = maps:get(cred_id, Info),
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    remove_credential(UserId, CredId);
handle_result(error, #{user_msg := UMsg}=Map, _Log, #{ action := revoke} ) ->
    %% a valid error response
    LogMsg = log_msg(Map, UMsg),
    return(error, #{user_msg => UMsg, log_msg => LogMsg});

%% PARAMETER handling
handle_result(ok,
              #{conf_params := ConfParams, request_params := ReqParams,
                version := Version} = Result,
              _Log, #{ action := parameter } )
  when is_list(ConfParams), is_list(ReqParams), is_binary(Version)  ->
    %% valid parameter response
    return(result, maps:with([conf_params, request_params, version], Result));
handle_result(error, Result, Log, #{ action := parameter } ) ->
    %% invalid parameter response
    LogMsg = io_lib:format("bad parameter response: ~p [~p]", [Result, Log]),
    UMsg = "the plugin returned a bad result, please contact the administrator",
    return(error, #{log_msg => LogMsg, user_msg => UMsg});

%% EVERYTHING ELSE => ERROR
handle_result(Result, Map, Log, Info) ->
    watts_plugin_error:maybe_send_mail_return_error(Result, Map, Log, Info).


%% @doc generate a log message, if needed from the user message
-spec log_msg(#{log_msg => binary(), _ => _}, binary()) -> binary().
log_msg(Map, UMsg) ->
    maps:get(log_msg, Map, io_lib:format("error response: ~p", [UMsg])).


%% @doc convert the result to the result data type.
-spec return(result |  oidc_login | error,
             map() | atom() | tuple()) -> result().
return(result, Result) ->
    {ok, Result};
return(oidc_login, Result) ->
    {oidc_login, Result};
return(error, Data) ->
    {error, Data}.

%% @doc convert the result of the plugin "ok", "error" or "oidc_login" to atom.
-spec result_to_atom(atom() | binary()) -> atom().
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



%% @doc serialize storing of the credential state in the database.
-spec sync_store_credential(binary(), binary(), binary(), binary()) ->
                                   {ok, binary()} | {error, Reason :: atom()}.
sync_store_credential(UserId, ServiceId, Interface, CredState) ->
    gen_server:call(?MODULE, {store_credential, UserId, ServiceId, Interface,
                              CredState}).

-record(state, {
         }).
-type state() :: #state{}.

%% @doc initialize the gen_server, doing nothing.
-spec init(noparams) -> {ok, state()}.
init(noparams) ->
    {ok, #state{}}.

%% @doc handle calls, the only one is to serialize storing or credential.
-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({store_credential, UserId, ServiceId, Interface, CredState}, _From,
            State) ->
    Result = store_credential(UserId, ServiceId, Interface,
                                          CredState),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%% @doc handle casts, the only one is to stop the gen_server.
-spec handle_cast(any(), state())
                 -> {noreply, state()} | {stop, normal, state()}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc nothing done here
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc nothing done here
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc nothing done here
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% functions with data access
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc lookup a credential for the user
-spec get_credential(binary(), binary())
                    -> {ok, watts:credential()} | {error, atom()}.
get_credential(UserId, CredentialId) ->
    watts_persistent:credential_fetch(CredentialId, UserId).

%% @doc get the number of credentials of user at service
-spec get_credential_count(binary(), binary()) -> {ok, integer()}.
get_credential_count(UserId, ServiceId) ->
    watts_persistent:credential_service_count(UserId, ServiceId).

%% @doc get the list of credenials for a user
-spec get_credential_list(binary()) -> {ok, [watts:credential()]}.
get_credential_list(UserId) ->
    watts_persistent:credential_fetch_list(UserId).

%% @doc store credential information.
%% The only information stored are:
%% <ul>
%% <li> the ID of credential (returned from persistent layer) </li>
%% <li> the ID of the user </li>
%% <li> the ID of the service </li>
%% <li> the interface that has been used </li>
%% <li> the state returned by the plugin </li>
%% </ul>
-spec store_credential(binary(), binary(), binary(), binary())
                      -> {ok, binary()} | {error, atom()}.
store_credential(UserId, ServiceId, Interface, CredentialState) ->
    SameStateAllowed = watts_service:allows_same_state(ServiceId),
    watts_persistent:credential_store(UserId, ServiceId, Interface,
                                   CredentialState, SameStateAllowed).

%% @doc delete the credential for the user
-spec remove_credential(binary(), binary()) -> result().
remove_credential(UserId, CredentialId) ->
    case watts_persistent:credential_delete(UserId, CredentialId) of
        ok ->
            return(result, #{});
        Error ->
            return(error, {deleting, Error})
    end.

%% @doc validate the credential entries returned from the plugin
-spec validate_credential_values([map()]) -> [cred_entry()].
validate_credential_values(Credential) ->
    validate_credential_values(Credential, []).

%% @doc validate the credential entries returned from the plugin
-spec validate_credential_values([map()], [cred_entry()]) -> [cred_entry()].
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

%% @doc convert a value to binary
-spec value_to_binary(any()) -> binary().
value_to_binary(Val) when is_binary(Val)->
    Val;
value_to_binary(Val) ->
    list_to_binary(io_lib:format("~p", [Val])).

%% @doc convert a value to file
-spec value_to_file(any()) -> binary().
value_to_file(Val) when is_binary(Val)->
    Val;
value_to_file(Val) when is_list(Val)->
    file_lines_to_binary(Val, <<>>);
value_to_file(Val) ->
    list_to_binary(io_lib:format("~p", [Val])).


%% @doc convert a list to binary
-spec file_lines_to_binary([any()], binary()) -> binary().
file_lines_to_binary([], File) ->
    File;
file_lines_to_binary([H | T], File) ->
    NewLine = value_to_binary(H),
    file_lines_to_binary(T, << File/binary, NewLine/binary >>).


%% @doc generate a new config map, ensuring no field is missing.
-spec config(map()) -> config().
config(#{action := _, service_id := _} = Config) ->
    BasicConfig = #{
      action =>  parameter,
      queue => undefined,
      user_id => undefined,
      user_info => undefined,
      cred_state => undefined,
      cred_id => undefined,
      params => #{}
     },
    maps:merge(BasicConfig, Config).


%% @doc create a minimal valid runner config
-spec runner_config(map()) -> watts_plugin_runner:config().
runner_config(#{action := _, service_id := _} = Config) ->
    BasicConfig = #{
      queue => undefined,
      user_info => undefined,
      cred_state => undefined,
      params => undefined},
    Keys = [action, service_id, queue, user_info, cred_state, params],
    maps:with(Keys, maps:merge(BasicConfig, Config)).

%% @doc run a plugin
-spec run_plugin(config()) -> watts_plugin_runner:result().
run_plugin(Config) ->
    {ok, Pid} = watts_plugin_sup:new_worker(),
    watts_plugin_runner:request_action(runner_config(Config), Pid).
