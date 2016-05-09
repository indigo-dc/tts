-module(tts_credential).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([
         get_list/1,
         get_count/2,
         exists/2,
         request/5,
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
    #{uid := UserId } = UserInfo,
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


handle_request_result({ok, #{error := Error}, Log}, _ServiceId, _UserInfo,
                      _Interface, _Token) ->
    return_error_with_debug({script, Error}, Log);
handle_request_result({ok, #{credential := Cred, state := CredState} , Log}
                      , ServiceId, #{uid := UserId} = UserInfo,
                      Interface, _Token) ->
    {ok, CredId} = sync_store_credential(UserId, ServiceId, Interface,
                                         CredState),
    lager:info("New Credential ~p [~p] at service ~p for ~p using ~p",
               [CredId, CredState, ServiceId, UserInfo, Interface]),
    Id = #{name => id, type => text, value => CredId},
    return_result_with_debug([ Id | Cred], Log);
handle_request_result({error, Error, Log}, _ServiceId, _UserInfo, _Interface,
                      _Token) ->
    return_error_with_debug({internal, Error}, Log);
handle_request_result({error, Error}, _ServiceId, _UserInfo, _Interface,
                      _Token) ->
    return_error_with_debug(Error, []).

handle_revoke_result({ok, #{error := Error}, Log}, _UserInfo, _CredId) ->
    return_error_with_debug({script, Error}, Log);
handle_revoke_result({ok, #{result := Result}, Log}, #{uid:=UserId}, CredId) ->
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



sync_store_credential(UserId, ServiceId, Interface, CredState) ->
    gen_server:call(?MODULE, {store_credential, UserId, ServiceId, Interface,
                              CredState}).

-record(state, {
         }).


init([]) ->
    {ok, #state{}}.

handle_call({store_credential, UserId, ServiceId, Interface, CredState}, _From,
            State) ->
    {ok, CredentialId} = store_credential(UserId, ServiceId, Interface,
                                          CredState),
    {reply, {ok, CredentialId}, State};
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
get_credential_count(#{uid := UserId}, ServiceId) ->
    get_credential_count(UserId, ServiceId);
get_credential_count(UserId, ServiceId) when is_binary(UserId) ->
    tts_data_sqlite:credential_get_count(UserId, ServiceId).

get_credential(CredId) ->
    tts_data_sqlite:credential_get(CredId).

get_credential_list(UserId) ->
    tts_data_sqlite:credential_get_list(UserId).

store_credential(UserId, ServiceId, Interface, CredentialState) ->
    tts_data_sqlite:credential_add(UserId, ServiceId, Interface,
                                   CredentialState).

remove_credential(UserId, CredentialId) ->
    tts_data_sqlite:credential_remove(UserId, CredentialId).


