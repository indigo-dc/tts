-module(tts_user).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([set_info/2]).
-export([connect_session/2]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          info = #{},
          sessions = []
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

set_info(Info, Pid) ->
    gen_server:call(Pid,{set_info,Info}).

connect_session(Session, Pid) ->
    gen_server:call(Pid,{connect_session,Session}).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({set_info,Info}, _From, State) ->
    {reply, ok, State#state{info=Info}};
handle_call({connect_session,Session}, _From,#state{sessions=Sessions} = State) ->
    MRef = monitor(process, Session),
    NewSessions = [ {Session, MRef} | lists:keydelete(Session,1,Sessions) ],
    {reply, ok, State#state{sessions=NewSessions}};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Object, _Info}, #state{sessions=Sessions}=State) ->
    NewSessions = remove_session_from_list(MonitorRef,Sessions),
    shutdown_if_done(NewSessions,State);
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

shutdown_if_done([],State) ->
    NewState = State#state{sessions=[]},
    case has_unexpired_token(State) of
        true ->
            {noreply,NewState};
        false ->
            {stop, normal, NewState}
    end;
shutdown_if_done(Sessions,State) ->
    {noreply,State#state{sessions=Sessions}}.

has_unexpired_token(_State) ->
    %TODO: implement
    false.


remove_session_from_list(MonitorRef,Sessions) ->
    lists:keydelete(MonitorRef,2,Sessions).

