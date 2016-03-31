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
          param_map = undefined,
          script = undefined
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

-spec lookup(list(), list(),pid()) -> ok.
lookup(Script, Params, Pid) ->
	gen_server:call(Pid, {lookup,Script,Params}).

-spec stop(pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).
%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({lookup,Script,ParamMap}, _From, State) ->
    {reply,ok,State#state{script=Script,param_map=ParamMap},1};
handle_call(stop, _From, State) ->
    {stop,normal,ok,State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout,#state{param_map=Map,script=Script}=State) ->
    Params = map_to_params(Map),
    Result = execute_script(Script,Params),
    tts_idh:user_result(Result),
    {noreply, State#state{script=undefined,param_map=undefined}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


execute_script(Script,Params) ->
    Cmd = create_command_line(Script,Params),
    StdOut = list_to_binary(os:cmd(Cmd)),
    case jsx:is_json(StdOut) of
        true -> jsx:decode(StdOut,[return_maps,{labels,attempt_atom}]);
        false -> #{error => bad_json_result, details => StdOut}
    end.

create_command_line(Script, []) ->
    Script;
create_command_line(Script, [Param|T]) ->
    CL = io_lib:format("~s ~s",[Script,Param]),
    create_command_line(CL,T).

map_to_params(#{type := openidconnect, subject := Subject, issuer := Issuer}) ->
    ["OpenIdConnect",Issuer,Subject].
    
