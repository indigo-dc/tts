-module(tts_idh).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([lookup_user/1]).
-export([user_result/1]).
-export([update_config/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("tts.hrl").
-define(IDH_CMD_MOD,"tts_idh_cmd").

-record(state, {
          configured = false,
          active_count = 0,
          queue = undefined,
          active = undefined,
          ready = undefined 
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec lookup_user(Map :: map()) -> {ok, Info :: map()} | {error, Reason :: term}.
lookup_user(Map) ->
    gen_server:call(?MODULE, {lookup_user, Map}).

-spec user_result(Map :: map()) -> ok.
user_result(Map) ->
    gen_server:call(?MODULE, {user_result, Map}).

-spec update_config() -> ok.
update_config() ->
	gen_server:call(?MODULE, update_config).
%% gen_server.

init([]) ->
    State = #state{
               queue = queue:new(),
               active = [],
               ready = [] 
              },
	{ok, State}.

handle_call({lookup_user, _Map}, _From, #state{configured=false}=State) ->
    {reply,{error, not_configured} , State};
handle_call({user_result, Map}, {Pid, _Tag}, State) ->
    NewState = handle_lookup_result(Map,Pid,State),
    {reply,ok,NewState};
handle_call({lookup_user, Map}, From, State) ->
    State2 = enqueue_lookup(Map, From, State),
    State3 = add_new_worker(State2),
    State4 = try_next_lookup(State3),
    {noreply,State4};
handle_call(update_config, _From, State) ->
    {Result, NewState} = update_config(State),
    {reply, Result, NewState};
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



enqueue_lookup(Map,From,#state{queue = Queue} = State) ->
    NewQueue = queue:in({Map, From},Queue),
    State#state{queue = NewQueue}.

add_new_worker(#state{ready=Ready, active_count=ActiveCount} = State) ->
    PossibleAndNeeded = ( (Ready == []) and 
                          (ActiveCount < ?CONFIG(idh_max_worker) ) ),
    add_new_worker(PossibleAndNeeded,State).

add_new_worker(false, State) ->
    State;
add_new_worker(true, #state{ready=Ready} = State) ->
    {ok,Pid} = tts_idh_worker_sup:start_worker(),
    MonRef = monitor(process,Pid),
    NewReady = [{Pid,MonRef} | Ready],
    State#state{ready=NewReady}.


try_next_lookup(#state{queue=Queue, ready=Ready, active_count=ActiveCount} = State) ->
    Possible = ( ( not queue:is_empty(Queue) ) and 
                 ( not Ready == [] ) and
                 ( ActiveCount < ?CONFIG(idh_max_worker))
               ),
    try_next_lookup(Possible,State). 

try_next_lookup(false,State) ->
    State;
try_next_lookup(true,State) ->
    process_next_lookup(State).

process_next_lookup(#state{queue=Queue, ready=Ready,
                           active=Active,active_count=ActiveCount} = State) ->
    {{value,{Map, From}},NewQueue} = queue:out(Queue),
    [{Pid,MonRef} | NewReady] = Ready,
    ok = tts_idh_worker:lookup(Map, ?IDH_CMD_MOD, Pid),
    NewActive = [{Pid,From,MonRef}|Active],
    State#state{queue=NewQueue,ready=NewReady,active=NewActive,active_count=ActiveCount+1}.


handle_lookup_result(Map,Pid,#state{active=Active, ready=Ready, active_count=ActiveCount} =State) ->
    {Pid,From,MonRef} = lists:keyfind(Pid,1,Active),
    NewActive = lists:keydelete(Pid,1,Active),
    gen_server:reply(From,{ok,Map}),
    NewActiveCount = ActiveCount -1,
    Reuse = NewActiveCount < ?CONFIG(idh_max_worker),
    NewReady = reuse_worker(Reuse,Pid,MonRef,Ready),
    State#state{active=NewActive, ready=NewReady, active_count=NewActiveCount}.

reuse_worker(true,Pid,MonRef,Ready) ->
    [{Pid, MonRef} | Ready];
reuse_worker(false,Pid,MonRef,Ready) ->
    demonitor(MonRef),
    tts_idh_worker:stop(Pid),
    Ready.


update_config(State) ->
    FileName = ?CONFIG(idh_cmd_file), 
    {ok, _} = erlydtl:compile_file(FileName, ?IDH_CMD_MOD),
    {ok, State#state{configured=true}}.

