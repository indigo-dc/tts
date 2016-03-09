-module(tts_credential).
-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
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

get_list(UserId) ->
    get_credential_list(UserId).

request(ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    Result = tts_cred_worker:request(ServiceId,UserInfo,Params,Pid),
    handle_request_result(Result,ServiceId,UserInfo,Token).

revoke(ServiceId, UserInfo) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    #{id := UserId } = UserInfo,
    case get_credential_state(UserId, ServiceId) of
        {ok, CredState} -> 
            tts_cred_worker:revoke(ServiceId,UserInfo,CredState,Pid);
        Other -> Other
    end.
    %% handle_request_result(Result,ServiceId,UserInfo,Token).

handle_request_result({ok,#{credential := Cred, state := _State}},ServiceId,
                      #{id := UserId},_Token) ->
    %TODO: ensure the user has no credential there (what about REST?)
    ok = store_credential_if_valid(UserId,ServiceId,Cred),     
    {ok,Cred};
handle_request_result({error,_},_ServiceId,_UserInfo,_Token) ->
    ok.


store_credential_if_valid(_,_,#{error := _}) ->
    % if the credential contains the error key, do not store it as there 
    % has been an error during creation
    ok;
store_credential_if_valid(UserId,ServiceId,#{state := CredState}) ->
    sync_store_credential(UserId,ServiceId,CredState).

sync_store_credential(UserId,ServiceId,CredState) ->
    gen_server:call(?MODULE,{store_credential, UserId, ServiceId, CredState}).

-record(state, {
}).


init([]) ->
	{ok, #state{}}.

handle_call({store_credential, UserId, ServiceId, CredState}, _From, State) ->
    ok = store_credential(UserId,ServiceId,CredState),
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
    case tts_data:credential_get(UserId) of
        {error, _ } -> 
            {ok, []};
        {ok, List} ->
            CredList = [ ServiceId || {ServiceId, _CredState} <- List ], 
            {ok, CredList}
    end.

get_credential_state(UserId,ServiceId) ->
    case tts_data:credential_get(UserId) of
        {error, _ } = Error ->  Error;
        {ok, List} ->
            case lists:keyfind(ServiceId,1,List) of
                false -> {error, not_found};
                {ServiceId, CredState} -> {ok, CredState}
            end
    end.


store_credential(UserId,ServiceId,CredentialState) ->
     tts_data:credential_add(UserId,ServiceId,CredentialState).
