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
          params = undefined,
          script = undefined
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec lookup(list(), list(),pid()) -> ok.
lookup(Script, Params, Pid) ->
	gen_server:call({lookup,Script,Params},Pid).

-spec stop(pid()) -> ok.
stop(Pid) ->
	gen_server:call(stop,Pid).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({lookup,Script,Params}, _From, State) ->
    gen_server:cast(perform_lookup,self()),
    {reply,ok,State#state{script=Script,params=Params}};
handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(perform_lookup,#state{params=undefined}=State) ->
    {noreply, State};
handle_cast(perform_lookup,#state{script=undefined}=State) ->
    {noreply, State};
handle_cast(perform_lookup,#state{params=Params,script=Script}=State) ->
    Result = execute_script(Script,Params),
    tts_idh:user_result(Result),
    {noreply, State#state{script=undefined,params=undefined}};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


execute_script(Script,Params) ->
    Cmd = create_command_line(Script,Params),
    StdOut = os:cmd(Cmd),
    case jsx:is_json(StdOut) of
        true -> jsx:decode(StdOut,[return_maps,{labels,attempt_atom}]);
        false -> #{error => bad_json_result}
    end.

create_command_line(Script, []) ->
    Script;
create_command_line(Script, [Param|T]) ->
    CL = io_lib:format("~s ~s",[Script,Param]),
    create_command_line(CL,T).
