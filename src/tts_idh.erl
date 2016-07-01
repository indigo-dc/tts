-module(tts_idh).
%%
%% Copyright 2016 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").

-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([lookup_user/1]).
-export([user_result/1]).
-export([reconfigure/0]).
-export([stop/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("tts.hrl").
-define(IDH_CMD_FILE, "idh_cmd_file").

-record(state, {
          configured = false,
          active_count = 0,
          queue = undefined,
          active = [],
          ready = [],
          script = undefined
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec lookup_user(Map :: map()) ->
    {ok, Info :: map()} | {error, Reason :: term}.
lookup_user(Map) ->
    gen_server:call(?MODULE, {lookup_user, Map}).

-spec user_result(Map :: map()) -> ok.
user_result(Map) ->
    gen_server:call(?MODULE, {user_result, Map}).

-spec reconfigure() -> ok.
reconfigure() ->
    gen_server:call(?MODULE, update_config).
%% gen_server.

init([]) ->
    State = #state{
               queue = queue:new()
              },
    {ok, State}.

handle_call({lookup_user, _Map}, _From, #state{configured=false}=State) ->
    {reply, {error, not_configured} , State};
handle_call({user_result, Map}, {Pid, _Tag}, State) ->
    NewState = handle_lookup_result(Map, Pid, State),
    {reply, ok, NewState};
handle_call({lookup_user, Map}, From, State) ->
    State2 = enqueue_lookup(Map, From, State),
    State3 = add_new_worker_if_needed(State2),
    trigger_next(),
    {noreply, State3};
handle_call(update_config, _From, State) ->
    NewState = update_config(State),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(perform_next, State) ->
    {noreply, try_next_lookup(State)};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{active = Active, ready = Ready}) ->
    stop_worker(Ready),
    stop_worker(Active),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



enqueue_lookup(Map, From, #state{queue = Queue} = State) ->
    NewQueue = queue:in({Map, From}, Queue),
    State#state{queue = NewQueue}.

add_new_worker_if_needed(State) ->
    #state{ready=Ready,
           active_count=ActiveCount} = State,
    PossibleAndNeeded = ( (Ready == []) and
                          (ActiveCount < ?CONFIG(idh_max_worker) ) ),
    add_new_worker(PossibleAndNeeded, State).

add_new_worker(false, State) ->
    State;
add_new_worker(true, #state{ready=Ready} = State) ->
    {ok, Pid} = tts_idh_worker_sup:start_worker(),
    MonRef = monitor(process, Pid),
    NewReady = [{Pid, MonRef} | Ready],
    State#state{ready=NewReady}.


try_next_lookup(State) ->
    #state{queue=Queue,
           ready=Ready,
           active_count=ActiveCount} = State,
    Possible = ( ( not queue:is_empty(Queue) ) and
                 ( not (Ready == [] ) ) and
                 ( ActiveCount < ?CONFIG(idh_max_worker))
               ),
    process_next_lookup(Possible, State).


process_next_lookup(true,
                    #state{queue=Queue, ready=Ready, active=Active,
                           active_count=ActiveCount, script=Script} = State) ->
    {{value, {Map, From}}, NewQueue} = queue:out(Queue),
    [{Pid, MonRef} | NewReady] = Ready,
    ok = tts_idh_worker:lookup(Script, Map, Pid),
    NewActive = [{Pid, From, MonRef}|Active],
    State#state{queue=NewQueue, ready=NewReady,
                active=NewActive, active_count=ActiveCount+1};
process_next_lookup(false, State) ->
    State.




handle_lookup_result(Map, Pid, State) ->
    #state{active=Active, ready=Ready, active_count=ActiveCount} = State,
    {Pid, From, MonRef} = lists:keyfind(Pid, 1, Active),
    NewActive = lists:keydelete(Pid, 1, Active),
    send_reply(From, Map),
    NewActiveCount = ActiveCount -1,
    Reuse = NewActiveCount < ?CONFIG(idh_max_worker),
    NewReady = reuse_worker(Reuse, Pid, MonRef, Ready),
    trigger_next(),
    State#state{active=NewActive, ready=NewReady, active_count=NewActiveCount}.

send_reply(From, #{uid := _,
                   uidNumber := _,
                   gidNumber:=_,
                   homeDirectory:=_,
                   userIds:=Mapping0}=Map) ->
    Mapping = convert_mapping(Mapping0, []),
    gen_server:reply(From, {ok, maps:put(userIds, Mapping, Map)});
send_reply(From, Map) ->
    gen_server:reply(From, {error, Map}).

convert_mapping([], Mapping) ->
    lists:reverse(Mapping);
convert_mapping([ {Issuer, Subject} | T ], Mapping) ->
    convert_mapping(T, [{Issuer, Subject} | Mapping]);
convert_mapping([ [Issuer, Subject] | T ], Mapping) ->
    convert_mapping(T, [{Issuer, Subject} | Mapping]).

reuse_worker(true, Pid, MonRef, Ready) ->
    [{Pid, MonRef} | Ready];
reuse_worker(false, Pid, MonRef, Ready) ->
    demonitor(MonRef),
    tts_idh_worker:stop(Pid),
    Ready.

trigger_next() ->
    gen_server:cast(?MODULE, perform_next).

update_config(State) ->
    ScriptName = ?CONFIG(idh_script),
    State#state{configured=true, script=ScriptName}.

stop_worker(List) ->
    Stop = fun(Item, Acc) ->
                   {Pid, MRef} = case Item of
                                     {P, M} ->  {P, M};
                                     {P, _, M} -> {P, M}
                                 end,
                   demonitor(MRef),
                   ok = tts_idh_worker:stop(Pid),
                   Acc
           end,
    lists:foldl(Stop, ok, List).

