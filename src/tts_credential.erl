-module(tts_credential).
-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([
         get_list/1,
         request/4,
         revoke/2,
         security_incident/1,
         security_incident/2
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

get_list(UserId) ->
    get_credential_list(UserId).

request(ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    Result = tts_cred_worker:request(ServiceId, UserInfo, Params, Pid),
    handle_request_result(Result, ServiceId, UserInfo, Token).

revoke(ServiceId, UserInfo) ->
    #{uid := UserId } = UserInfo,
    case get_credential_state(UserId, ServiceId) of
        {ok, CredState} ->
            {ok, Pid} = tts_cred_sup:new_worker(),
            Result=tts_cred_worker:revoke(ServiceId, UserInfo, CredState, Pid),
            handle_revoke_result(Result, ServiceId, UserInfo, CredState);
        Other -> Other
    end.

security_incident( UserInfo) ->
    %TODO: make this run in parallel
    #{uid := UserId } = UserInfo,
    {ok, List} = get_credential_states(UserId),
    Incident = fun({ServiceId, CredState}, ResultList) ->
                       {ok, Pid} = tts_cred_sup:new_worker(),
                       Result = tts_cred_worker:security_incident(ServiceId,
                                                                  UserInfo,
                                                                  CredState,
                                                                  Pid),
                       [{Result, ServiceId, CredState} | ResultList]
               end,
    ResultList = lists:foldl(Incident, [], List),
    HandleResult = fun({Result, ServiceId, CredState}, _) ->
                           handle_incident_result(Result, ServiceId,
                                                  UserInfo, CredState)
                   end,
    lists:foldl(HandleResult, [], ResultList),
    ok.


security_incident(ServiceId, UserInfo) ->
    #{uid := UserId } = UserInfo,
    case get_credential_state(UserId, ServiceId) of
        {ok, CredState} ->
            {ok, Pid} = tts_cred_sup:new_worker(),
            Result = tts_cred_worker:security_incident(ServiceId, UserInfo,
                                                       CredState, Pid),
            handle_incident_result(Result, ServiceId, UserInfo, CredState);
        Other -> Other
    end.

handle_request_result({ok, #{credential := Cred} = CredMap, Log}, ServiceId,
                      #{uid := UserId}, _Token) ->
    %TODO: ensure the user has no credential there (what about REST?)
    %TODO: write logs to file and pass the info to the user, so admins know
    %about it
    ok = store_credential_if_valid(UserId, ServiceId, CredMap),
    return_result_with_debug(Cred, Log);
handle_request_result({error, _, Log}, _ServiceId, _UserInfo, _Token) ->
    Cred = false,
    return_result_with_debug(Cred, Log).


handle_revoke_result({ok, #{result := Result}, Log}, ServiceId,
                     #{uid := UserId}, CredState) ->
    ok = remove_credential(UserId, ServiceId, CredState),
    return_result_with_debug(Result, Log);
handle_revoke_result({error, _}, _ServiceId, _UserInfo, _CredState) ->
    Result = {error, revoke_failed},
    return_result_with_debug(Result, []).

return_result_with_debug(Result, Log) ->
    return_result_with_debug(Result, Log, ?DEBUG_MODE).

return_result_with_debug(Result, Log, true) ->
    {ok, Result, Log};
return_result_with_debug(Result, _Log, false) ->
    {ok, Result, []}.


handle_incident_result({ok, #{result := Result}}, ServiceId,
                       #{uid := UserId}, CredState) ->
    ok = remove_credential(UserId, ServiceId, CredState),
    {ok, Result};
handle_incident_result({error, _}, _ServiceId, _UserInfo, _CredState) ->
    ok.

store_credential_if_valid(_, _, #{error := _}) ->
    % if the credential contains the error key, do not store it as there
    % has been an error during creation
    ok;
store_credential_if_valid(UserId, ServiceId, #{state := CredState}) ->
    sync_store_credential(UserId, ServiceId, CredState).

sync_store_credential(UserId, ServiceId, CredState) ->
    gen_server:call(?MODULE, {store_credential, UserId, ServiceId, CredState}).

-record(state, {
         }).


init([]) ->
    {ok, #state{}}.

handle_call({store_credential, UserId, ServiceId, CredState}, _From, State) ->
    ok = store_credential(UserId, ServiceId, CredState),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% functions with data access
get_credential_list(UserId) ->
    {ok, List} = get_credential_states(UserId),
    CredList = [ ServiceId || {ServiceId, _CredState} <- List ],
    {ok, CredList}.

get_credential_state(UserId, ServiceId) ->
    {ok, List} = get_credential_states(UserId),
    case lists:keyfind(ServiceId, 1, List) of
        false -> {error, not_found};
        {ServiceId, CredState} -> {ok, CredState}
    end.

%% % functions using ets
%%
%% get_credential_states(UserId) ->
%%     case tts_data:credential_get(UserId) of
%%         {error, _ } ->
%%             {ok, []};
%%         {ok, List} ->
%%             {ok, List}
%%     end.
%%
%%
%% store_credential(UserId, ServiceId, CredentialState) ->
%%      tts_data:credential_add(UserId, ServiceId, CredentialState).
%%
%% remove_credential(UserId, ServiceId, CredentialState) ->
%%      tts_data:credential_remove(UserId, ServiceId, CredentialState).
%%

% functions using sqlite

get_credential_states(UserId) ->
    tts_data_sqlite:credential_get(UserId).

store_credential(UserId, ServiceId, CredentialState) ->
    tts_data_sqlite:credential_add(UserId, ServiceId, CredentialState).

remove_credential(UserId, ServiceId, CredentialState) ->
    tts_data_sqlite:credential_remove(UserId, ServiceId, CredentialState).


