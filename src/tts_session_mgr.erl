-module(tts_session_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([new_session/0]).
-export([get_session/1]).
-export([session_closing/1]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          ets = none
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec new_session() -> {ok, pid()}.
new_session() -> 
    gen_server:call(?MODULE, new_session).

-spec get_session(ID :: uuid:uuid()) -> {ok, pid()} | {error, not_found}.
get_session(ID) ->
    lookup_session_pid(ID).

-spec session_closing(ID :: binary()) -> ok.
session_closing(ID) ->
    delete_session(ID).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call(new_session, _From, State) ->
    ID = get_unique_id(State),
    Pid = start_session(ID),
    ok = set_session_for_id(ID,Pid),
    {reply, {ok, Pid}, State};
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




get_unique_id(State) ->
    ID = tts_utils:random_string(64), 
    repeat_id_gen_if_needed(add_new_session_entry(ID, State), ID, State).

start_session(ID) ->
    {ok, Pid} = tts_session_sup:new_session(ID),
    Pid.

repeat_id_gen_if_needed(ok,ID,_State) ->
    ID;
repeat_id_gen_if_needed(_,_ID,State) ->
    get_unique_id(State).


%% 
%% functions with data access
%%

add_new_session_entry(ID,_State) ->
   tts_data:sessions_create_new(ID). 

lookup_session_pid(ID) ->
   tts_data:sessions_get_pid(ID). 

set_session_for_id(ID,Pid) ->
   tts_data:sessions_update_pid(ID,Pid). 

delete_session(ID) ->
    tts_data:sessions_delete(ID),
    ok.
