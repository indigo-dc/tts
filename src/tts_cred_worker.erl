-module(tts_cred_worker).
-behaviour(gen_server).

%% API.
-define(TIMEOUT,20000).

-export([start_link/0]).
-export([request/5]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          action = undefined,
          client = undefined,
          service_id = undefined,
          user_info = undefined,
          token = undefined,
          params = undefined
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec request(ServiceId :: any(), UserInfo :: map(), Token :: map(),
              Params::any(), Pid::pid()) -> {ok, pid()}.
request(ServiceId,UserInfo,Token,Params,Pid) ->
	gen_server:call(Pid, {request_credential, ServiceId, UserInfo, Token,
                          Params},infinity).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({request_credential,ServiceId,UserInfo,Token,Params}, From, #state{client = undefined} = State) ->
    NewState = State#state{ action = request,
                            client = From,
                            service_id = ServiceId,
                            user_info = UserInfo,
                            token = Token,
                            params = Params
                          }, 
    gen_server:cast(self(),perform_request),
    {noreply, NewState,200};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(perform_request, State) ->
    perform_request(State),
    {stop, normal, State}; 
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
    {ok,NewState} = send_reply({error,timeout},State),
    {stop,normal,NewState};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
    {ok,_NewState} = send_reply({error, internal},State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


send_reply(_Reply,#state{client=undefined}=State) ->
    {ok,State};
send_reply(Reply,#state{client=Client}=State) ->
    gen_server:reply(Client,Reply),
    {ok,State#state{client=undefined}}.


perform_request(State) ->
    #state{service_id = ServiceId
          } = State,
    {ok, _ServiceInfo} = tts_service:get_info(ServiceId),
    ok. 

                      
