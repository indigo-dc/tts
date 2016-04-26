-module(tts_credential).
-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([
         get_list/1,
         request/4,
         revoke/2
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

-spec get_list(binary()) -> {ok, [{ServiceId::binary(), CredState::binary()}]}.
get_list(UserId) ->
    get_credential_list(UserId).

-spec request(binary(), map(), map(), list()) ->
    {ok, map(), list()} | {error, any(), list()}.
request(ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    Result = tts_cred_worker:request(ServiceId, UserInfo, Params, Pid),
    handle_request_result(Result, ServiceId, UserInfo, Token).

-spec revoke(binary(), map()) ->
    {ok, map(), list()} | {error, any(), list()}.
revoke(ServiceId, UserInfo) ->
    #{uid := UserId } = UserInfo,
    case get_credential_state(UserId, ServiceId) of
        {ok, CredState} ->
            {ok, Pid} = tts_cred_sup:new_worker(),
            Result=tts_cred_worker:revoke(ServiceId, UserInfo, CredState, Pid),
            handle_revoke_result(Result, ServiceId, UserInfo, CredState);
        Other -> Other
    end.


handle_request_result({ok, #{error := Error}, Log}, _ServiceId, _Uid, _Token) ->
    return_error_with_debug({script, Error}, Log);
handle_request_result({ok, #{credential := Cred, state := CredState} , Log}
                      , ServiceId, #{uid := UserId}, _Token) ->
    %TODO: write logs to file and pass the info to the user, so admins know
    %about it
    ok = sync_store_credential(UserId, ServiceId, CredState),
    return_result_with_debug(Cred, Log);
handle_request_result({error, Error, Log}, _ServiceId, _UserInfo, _Token) ->
    return_error_with_debug({internal, Error}, Log);
handle_request_result({error, Error}, _ServiceId, _UserInfo, _Token) ->
    return_error_with_debug(Error, []).

handle_revoke_result({ok, #{error := Error}, Log}, _ServiceId,
                     _UserInfo, _CredState) ->
    return_error_with_debug({script, Error}, Log);
handle_revoke_result({ok, #{result := Result}, Log}, ServiceId,
                     #{uid := UserId}, CredState) ->
    ok = remove_credential(UserId, ServiceId, CredState),
    return_result_with_debug(Result, Log);
handle_revoke_result({error, Error, Log}, _ServiceId, _UserInfo, _CredState) ->
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


% functions with data access

get_credential_states(UserId) ->
    tts_data_sqlite:credential_get(UserId).

store_credential(UserId, ServiceId, CredentialState) ->
    tts_data_sqlite:credential_add(UserId, ServiceId, CredentialState).

remove_credential(UserId, ServiceId, CredentialState) ->
    tts_data_sqlite:credential_remove(UserId, ServiceId, CredentialState).


