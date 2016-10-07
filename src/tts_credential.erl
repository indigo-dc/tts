-module(tts_credential).
%%
%% Copyright 2016 SCC/KIT
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
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([
         get_list/1,
         get_count/2,
         exists/2,
         request/5,
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

-spec get_list(binary()) -> {ok, [tts:cred()]}.
get_list(UserId) ->
    get_credential_list(UserId).

-spec get_count(binary(), binary()) -> {ok, non_neg_integer()}.
get_count(UserId, ServiceId) ->
    get_credential_count(UserId, ServiceId).

-spec exists(binary(), binary()) -> true | false.
exists(UserId, CredId) ->
    case get_credential(UserId, CredId) of
        {ok, _} -> true;
        _ -> false
    end.

-spec request(binary(), tts:user_info(), binary(), map()|atom(), list()) ->
    {ok, map(), list()} | {error, any(), list()}.
request(ServiceId, UserInfo, Interface, Token, Params) ->
    {ok, Limit} = tts_service:get_credential_limit(ServiceId),
    {ok, Count} = get_credential_count(UserInfo, ServiceId),
    Enabled = tts_service:is_enabled(ServiceId),
    case {Enabled, Count < Limit } of
        {true, true} ->
            {ok, Pid} = tts_cred_sup:new_worker(),
            Result = tts_cred_worker:request(ServiceId, UserInfo, Params, Pid),
            handle_request_result(Result, ServiceId, UserInfo, Interface,
                                  Token);
        {false, _} ->
            {error, service_disabled, []};
        {true, false} ->
            {error, limit_reached, []}
    end.

-spec revoke(binary(), tts:user_info ()) ->
    {ok, tts:cred(), list()} | {error, any(), list()}.
revoke(CredentialId, UserInfo) ->
    #{userid := UserId } = UserInfo,
    case get_credential(UserId, CredentialId) of
        {ok, Cred} ->
            #{ service_id := ServiceId,
               cred_state := CredState,
               cred_id := CredId
             } = Cred,
            {ok, Pid} = tts_cred_sup:new_worker(),
            Result=tts_cred_worker:revoke(ServiceId, UserInfo, CredState, Pid),
            handle_revoke_result(Result, UserInfo, CredId);
        {error, Reason} -> {error, Reason, []}
    end.


get_params(ServiceId) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    Result = tts_cred_worker:get_params(ServiceId, Pid),
    handle_params_result(Result).



handle_request_result({ok, #{error := Error}, Log}, _ServiceId, _UserInfo,
                      _Interface, _Token) ->
    return_error_with_debug({script, Error}, Log);
handle_request_result({ok, #{credential := Cred0, state := CredState}, Log}
                      , ServiceId, #{userid := UserId } = UserInfo,
                      Interface, _Token) ->
    Cred = validate_credential_values(Cred0),
    case sync_store_credential(UserId, ServiceId, Interface, CredState) of
        {ok, CredId} ->
            lager:info("New Credential ~p [~p] at service ~p for ~p using ~p",
                       [CredId, CredState, ServiceId, UserInfo, Interface]),
            return_result_with_debug(#{id => CredId, entries => Cred}, Log);
        Error ->
            return_error_with_debug({storing, Error}, Log)
    end;
handle_request_result({ok, #{credential := _Cred}, Log} , ServiceId, UserInfo,
                      _Interface, _Token) ->
    lager:critical("missing state in service ~p for ~p", [ServiceId, UserInfo]),
    return_error_with_debug({missing_state, ServiceId}, Log);
handle_request_result({error, Error, Log}, _ServiceId, _UserInfo, _Interface,
                      _Token) ->
    return_error_with_debug({internal, Error}, Log);
handle_request_result({error, Error}, _ServiceId, _UserInfo, _Interface,
                      _Token) ->
    return_error_with_debug(Error, []).

handle_revoke_result({ok, #{error := Error}, Log}, _UserInfo, _CredId) ->
    return_error_with_debug({script, Error}, Log);
handle_revoke_result({ok, #{result := Result}, Log}, #{userid:=UserId},
                     CredId) ->
    ok = remove_credential(UserId, CredId),
    return_result_with_debug(Result, Log);
handle_revoke_result({error, Error, Log}, _UserInfo, _CredId) ->
    return_error_with_debug({internal, Error}, Log).

return_result_with_debug(Result, Log) ->
    return_result_with_debug(Result, Log, ?DEBUG_MODE).

return_result_with_debug(Result, Log, true) ->
    {ok, Result, Log};
return_result_with_debug(Result, _Log, false) ->
    {ok, Result, []}.

return_error_with_debug(Error, Log) ->
    return_error_with_debug(Error, Log, ?DEBUG_MODE).

return_error_with_debug(Error, Log, true) ->
    {error, Error, Log};
return_error_with_debug(Error, _Log, false) ->
    {error, Error, []}.

handle_params_result({ok, #{conf_params := ConfParams,
                            request_params := RequestParams}, _Log}) ->
    {ok, ConfParams, RequestParams};
handle_params_result({ok, #{conf_params := ConfParams}, _Log}) ->
    {ok, ConfParams, []};
handle_params_result({ok, #{request_params := RequestParams}, _Log}) ->
    {ok, [], RequestParams};
handle_params_result(_) ->
    {error, bad_result}.





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
get_credential_count(#{userid := UserId}, ServiceId) ->
    get_credential_count(UserId, ServiceId);
get_credential_count(UserId, ServiceId) when is_binary(UserId) ->
    tts_data_sqlite:credential_get_count(UserId, ServiceId).

get_credential(CredId) ->
    tts_data_sqlite:credential_get(CredId).

get_credential_list(UserId) ->
    tts_data_sqlite:credential_get_list(UserId).

store_credential(UserId, ServiceId, Interface, CredentialState) ->
    SameStateAllowed = tts_service:allows_same_state(ServiceId),
    tts_data_sqlite:credential_add(UserId, ServiceId, Interface,
                                   CredentialState, SameStateAllowed).

remove_credential(UserId, CredentialId) ->
    tts_data_sqlite:credential_remove(UserId, CredentialId).


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
