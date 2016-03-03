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
          service_info = undefined,
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
    gen_server:cast(self(),get_service_info),
    {noreply, NewState,200};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(perform_request, State) ->
    {ok, NewState} = perform_request(State),
    {stop, normal, NewState}; 
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
    {ok,NewState} = send_reply({error,timeout},State),
    {stop,normal,NewState};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, State) ->
    {ok,_NewState} = send_reply({error, {internal,Reason}},State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


send_reply(_Reply,#state{client=undefined}=State) ->
    {ok,State};
send_reply(Reply,#state{client=Client}=State) ->
    gen_server:reply(Client,Reply),
    {ok,State#state{client=undefined}}.

% This is the main working function
perform_request(State) ->
    #state{service_id = ServiceId,
           user_info = UserInfo
          } = State,
    {ok, ServiceInfo} = tts_service:get_info(ServiceId),
    {ok, Connection} = connect_to_service(ServiceInfo),
    {ok, CmdMod} = get_cmd_module(request,ServiceInfo),
    {ok, Result} = execute_commands(CmdMod,UserInfo,Connection),
    ok = close_connection(Connection),
    send_reply(Result,State).

                      
connect_to_service(#{con_type := ssh , con_host := _Host, con_key_file := _KeyFile} = Info ) ->
    _Port = maps:get(con_port,Info,22),
    _User = maps:get(con_user,Info,<<"root">>),
    Connection = con,
    {ok, Connection};
connect_to_service( _ )  ->
    throw(unknown_con_type).

get_cmd_module(request,#{cmd_mod_req := CmdMod}) ->
    {ok, CmdMod};
get_cmd_module(revoke,#{cmd_mod_rev := CmdMod}) ->
    {ok, CmdMod};
get_cmd_module(incident,#{cmd_mod_si := CmdMod}) ->
    {ok, CmdMod};
get_cmd_module(_, _) ->
    {error, unknown_cmd_mod}.


execute_commands(undefined,_UserInfo, _Connection) ->
    {error, no_cmd_mod};
execute_commands(CmdMod,_UserInfo, _Connection) when is_atom(CmdMod) ->
    {ok, IoList} = CmdMod:render(), 
    io:format("got the rendered file ~p~n",[IoList]),
    {ok, IoList};
execute_commands(_Mod,_Info, _Connection) ->
    {error, no_cmd_mod}.

close_connection(_Con) ->
    ok.
