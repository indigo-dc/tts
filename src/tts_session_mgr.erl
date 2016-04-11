-module(tts_session_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([new_session/0]).
-export([get_session/1]).
-export([close_all_sessions/0]).
-export([session_wants_to_close/1]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec new_session() -> {ok, pid()}.
new_session() ->
    gen_server:call(?MODULE, new_session).

-spec get_session(ID :: uuid:uuid() | undefined) -> {ok, pid()}.
get_session(undefined) ->
    new_session();
get_session(ID) ->
    lookup_or_create_session_pid(ID).

-spec session_wants_to_close(ID :: binary()) -> ok.
session_wants_to_close(ID) ->
    gen_server:call(?MODULE, {delete_session, ID}).

-spec close_all_sessions() -> ok.
close_all_sessions() ->
    gen_server:call(?MODULE, close_all_sessions).


%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(new_session, _From, State) ->
    ID = get_unique_id(),
    Pid = start_session(ID),
    ok = set_session_for_id(ID, Pid),
    {reply, {ok, Pid}, State};
handle_call({delete_session, ID}, _From, State) ->
    {ok, Pid} = lookup_session_pid(ID),
    delete_session(ID),
    tts_session:close(Pid),
    {reply, ok, State};
handle_call(close_all_sessions, _From, State) ->
    SessionList = get_all_sessions(),
    delete_sessions(SessionList),
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


delete_sessions([]) ->
    ok;
delete_sessions([#{id:= Id, pid:= Pid}|T]) ->
    delete_session(Id),
    tts_session:close(Pid),
    delete_sessions(T).


get_unique_id() ->
    ID = tts_utils:random_string(64),
    repeat_id_gen_if_needed(add_new_session_entry(ID)).

start_session(ID) ->
    {ok, Pid} = tts_session_sup:new_session(ID),
    Pid.

repeat_id_gen_if_needed({ok, ID}) ->
    ID;
repeat_id_gen_if_needed(_) ->
    get_unique_id().

lookup_or_create_session_pid({ok, Pid}) ->
    {ok, Pid};
lookup_or_create_session_pid({error, _}) ->
    new_session();
lookup_or_create_session_pid(ID) ->
    lookup_or_create_session_pid(lookup_session_pid(ID)).

%%
%% functions with data access
%%
get_all_sessions() ->
    tts_data:sessions_get_list().

add_new_session_entry(ID) ->
    case tts_data:sessions_create_new(ID) of
        ok -> {ok, ID};
        _ -> {error, used}
    end.

lookup_session_pid(ID) ->
    tts_data:sessions_get_pid(ID).

set_session_for_id(ID, Pid) ->
    tts_data:sessions_update_pid(ID, Pid).

delete_session(ID) ->
    tts_data:sessions_delete(ID),
    ok.
