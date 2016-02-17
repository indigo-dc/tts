-module(tts_service_worker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([perform_request/5]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          service_id = undefined,
          token = undefined,
          user_info = undefined,
          ref = undefined
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

perform_request(ServiceId,UserId,Token,Params,Pid) ->
    %TODO, maybe change the timeout
	gen_server:call(Pid,{request,ServiceId, UserId, Token, Params}).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({request, _ServiceId, _UserId, _Token, _Params}, _From, State) ->
	{reply, {ok, #{}}, State};
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
