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
          user_info = undefined,
          token = undefined,
          params = undefined,
          connection = undefined,
          con_type = undefined,
          cmd_list = [],
          cmd_log = [],
          cmd_state = undefined,
          error = undefined
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
    gen_server:cast(self(),perform_request),
    {noreply, NewState,200};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(perform_request, State) ->
    {ok, NewState} = prepare_request(State),
    trigger_next_command(),
    {noreply, NewState}; 
handle_cast(execute_cmd, State) ->
    execute_single_command_or_exit(State);
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({ssh_cm,SshConRef,SshMsg}, State) ->
    handle_ssh_result(SshConRef,SshMsg,State);
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


prepare_request(State) ->
    #state{service_id = ServiceId,
           user_info = UserInfo
          } = State,
    {ok, ServiceInfo} = tts_service:get_info(ServiceId),
    Connection = connect_to_service(ServiceInfo),
    {ok, CmdMod} = get_cmd_module(request,ServiceInfo),
    create_command_list_and_update_state(CmdMod,UserInfo,ServiceInfo,Connection,State).

    %% ok = close_connection(Connection,ServiceInfo),
    %% send_reply(Result ,State).

                      
connect_to_service(#{con_type := local}) ->
    {ok, local};
connect_to_service(#{con_type := ssh , con_host := Host } = Info ) ->
    Port = maps:get(con_port,Info,22),
    User = maps:get(con_user,Info,"root"),
    UserDir = maps:get(con_ssh_user_dir,Info),
    ssh:connect(Host,Port,[{user_dir,UserDir},{user_interaction,false},{user,User},{id_string,"TokenTranslationService"}],10000);
connect_to_service(#{con_type := ssh } ) ->
    throw(missing_ssh_config);
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


create_command_list_and_update_state(undefined,_UserInfo, #{con_type := ConType}, _Connection, State) ->
    {ok, State#state{error = no_cmd_mod, con_type = ConType}}; 
create_command_list_and_update_state(CmdMod, UserInfo, #{con_type := ConType}, {ok, Connection},State) when is_atom(CmdMod) ->
    #{ uid := User,
       uidNumber := Uid, 
       gidNumber := Gid,
       homeDirectory := HomeDir
     } = UserInfo,
    {ok, IoList} = CmdMod:render([{user, User },{uid, Uid},{gid, Gid},{home_dir, HomeDir}]), 
    CmdList = binary:split(list_to_binary(IoList),[<<"\n">>],[global]),
    {ok, State#state{cmd_list=CmdList, connection = Connection, con_type = ConType}};
create_command_list_and_update_state(_Mod, _Info, #{con_type := ConType}, _Connection, State) ->
    {ok, State#state{error = connection_or_cmd_error, con_type = ConType}}. 

trigger_next_command() ->
    gen_server:cast(self(),execute_cmd).

execute_single_command_or_exit(State) ->
    #state{ cmd_list = CmdList,
            connection = Connection,
            error = Error,
            con_type = ConType,
            cmd_log = CmdLog
          } = State,
    execute_command_or_send_result(CmdList,ConType,Connection,Error,CmdLog,State).

execute_command_or_send_result([],ConType,Connection,undefined,Log,State) ->
    close_connection(Connection,ConType),
    [LastResult|_] = Log,
    {ok,NewState} = send_reply({ok, LastResult},State),
    {stop,normal,NewState};
execute_command_or_send_result([Cmd|T],ConType,Connection,undefined,_Log,State) ->
    {ok, CmdState} = execute_command(Cmd,ConType,Connection),
    {noreply, State#state{cmd_state = CmdState}, cmd_list = T}.


execute_command(Cmd, ssh, Connection) when is_list(Cmd) ->
    {ok, ChannelId} = ssh_connection:session_channel(Connection,infinity),
    success = ssh_connection:exec(Connection,ChannelId,Cmd,infinity),
    {ok, #{channel_id => ChannelId, cmd => Cmd}};
execute_command(Cmd, ssh, Connection) when is_binary(Cmd) ->
    execute_command(binary_to_list(Cmd), ssh, Connection).


handle_ssh_result(SshCon,SshMsg, #state{connection = SshCon} = State) ->
    #state{
       cmd_state = #{ channel_id := ChannelId}
      } = State,
    {ok, NewState} = handle_ssh_message(SshMsg,ChannelId,State),
    {noreply, NewState};
handle_ssh_result(_SshCon,_SshMsg,State) ->
    {noreply,State}.

handle_ssh_message({data,ChannelId,0,Data},ChannelId,State) ->
    % data on std out
    #state{ cmd_state = CmdState
          } = State,
    StdOut = [Data | maps:get(std_out,CmdState,[])],
    StdErr = [<<>> | maps:get(std_err,CmdState,[])],
    Update = #{std_out => StdOut, std_err => StdErr},
    NewState = State#state{ cmd_state = maps:merge(CmdState,Update) },
    {ok, NewState};
handle_ssh_message({data,ChannelId,1,Data},ChannelId,State) ->
    % data on std err 
    #state{ cmd_state = CmdState
          } = State,
    StdOut = [<<>> | maps:get(std_out,CmdState,[])],
    StdErr = [Data | maps:get(std_err,CmdState,[])],
    Update = #{std_out => StdOut, std_err => StdErr},
    NewState = State#state{ cmd_state = maps:merge(CmdState,Update) },
    {ok, NewState};
handle_ssh_message({eof,ChannelId},ChannelId,State) ->
    {ok, State};
handle_ssh_message({exit_signal,ChannelId, ExitSignal, ErrorMsg, Lang},ChannelId,State) ->
    #state{ cmd_state = CmdState
          } = State,
    Update = #{exit_signale => ExitSignal, err_msg => ErrorMsg, lang => Lang},
    NewState = State#state{ cmd_state = maps:merge(CmdState,Update) },
    {ok, NewState};
handle_ssh_message({exit_status,ChannelId, ExitStatus},ChannelId,State) ->
    #state{ cmd_state = CmdState
          } = State,
    Update = #{exit_status => ExitStatus},
    NewState = State#state{ cmd_state = maps:merge(CmdState,Update) },
    {ok, NewState};
handle_ssh_message({closed,ChannelId},ChannelId,State) ->
    #state{ cmd_state = CmdState,
            cmd_log = Log
          } = State,
    trigger_next_command(),
    NewState = State#state{ cmd_state = #{},
                            cmd_log = [ CmdState | Log] },
    {ok, NewState};
handle_ssh_message(_Msg,_ChannelId,State) ->
    {ok, State}.



close_connection(Connection, ssh) ->
    ok = ssh:close(Connection);
close_connection(_,  local ) ->
    ok;
close_connection(_Con,_Info) ->
    ok.

send_reply(_Reply,#state{client=undefined}=State) ->
    {ok,State};
send_reply(Reply,#state{client=Client}=State) ->
    gen_server:reply(Client,Reply),
    {ok,State#state{client=undefined}}.

