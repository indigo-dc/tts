-module(tts_idh_worker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([lookup/3]).
-export([stop/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          map = undefined,
          module = undefined
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec lookup(map(),atom(),pid()) -> ok.
lookup(Map, Module, Pid) ->
	gen_server:call({lookup,Map,Module},Pid).

-spec stop(pid()) -> ok.
stop(Pid) ->
	gen_server:call(stop,Pid).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({lookup,Map,Module}, _From, State) ->
    gen_server:cast(perform_lookup,self()),
    {reply,ok,State#state{map=Map,module=Module}};
handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(perform_lookup,#state{map=undefined}=State) ->
    {noreply, State};
handle_cast(perform_lookup,#state{module=undefined}=State) ->
    {noreply, State};
handle_cast(perform_lookup,#state{map=Map,module=Module}=State) ->
    CmdList = create_cmd_list(Module,Map), 
    Result = execute_commands(CmdList),
    tts_idh:user_result(Result),
    {noreply, State#state{map=undefined,module=undefined}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

execute_commands(_CmdList) ->
    %TODO:implement
    ok.

create_cmd_list(CmdMod,Map) ->
    {ok, IoList} = CmdMod:render([{info, Map}]),
    CmdList = binary:split(list_to_binary(IoList),[<<"\n">>],[global, trim_all]),
    RemoveComments = fun(Line,Cmds) ->
                             case  binary:first(Line) of
                                 $# -> Cmds;
                                 _ -> [Line | Cmds]
                             end
                     end,
    lists:reverse(lists:foldl(RemoveComments,[],CmdList)).
