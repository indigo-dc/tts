-module(tts_cred_worker).
-behaviour(gen_server).

%% API.
-define(TIMEOUT, 20000).

-export([start_link/0]).
-export([start/0]).
-export([stop/1]).
-export([request/4]).
-export([revoke/4]).

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
          params = undefined,
          cred_state = undefined,
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

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start(?MODULE, [], []).

-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec request(ServiceId :: any(), UserInfo :: map(), Params::any(), pid()) ->
    {ok, map(), list()} | {error, any(), list()} | {error, atom()}.
request(ServiceId, UserInfo, Params, Pid) ->
    gen_server:call(Pid, {request_credential, ServiceId, UserInfo,
                          Params}, infinity).

-spec revoke(ServiceId :: any(), UserInfo :: map(), CredState::any(), pid()) ->
    {ok, map(), list()} | {error, any(), list()} | {error, atom()}.
revoke(ServiceId, UserInfo, CredState, Pid) ->
    gen_server:call(Pid, {revoke_credential, ServiceId, UserInfo,
                          CredState}, infinity).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({request_credential, ServiceId, UserInfo, Params}, From,
            #state{client = undefined} = State) ->
    NewState = State#state{ action = request,
                            client = From,
                            service_id = ServiceId,
                            user_info = UserInfo,
                            params = Params
                          },
    gen_server:cast(self(), perform_action),
    {noreply, NewState, 200};
handle_call({revoke_credential, ServiceId, UserInfo, CredState}, From,
            #state{client = undefined} = State) ->
    NewState = State#state{ action = revoke,
                            client = From,
                            service_id = ServiceId,
                            user_info = UserInfo,
                            cred_state = CredState
                          },
    gen_server:cast(self(), perform_action),
    {noreply, NewState, 200};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(perform_action, State) ->
    {ok, NewState} = prepare_action(State),
    trigger_next_command(),
    {noreply, NewState};
handle_cast(execute_cmd, State) ->
    execute_single_command_or_exit(State);
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ssh_cm, SshCon, SshMsg}
            , #state{connection=SshCon,
                     cmd_state = #{ channel_id := ChannelId}}=State) ->
    {ok, NewState} = handle_ssh_message(SshMsg, ChannelId, State),
    {noreply, NewState};
handle_info(timeout, State) ->
    {ok, NewState} = send_reply({error, timeout}, State),
    {stop, normal, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    {ok, _NewState} = send_reply({error, {internal, Reason}}, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


prepare_action(State) ->
    #state{service_id = ServiceId,
           user_info = UserInfo
          } = State,
    {ok, ServiceInfo} = tts_service:get_info(ServiceId),
    Connection = connect_to_service(ServiceInfo),
    {ok, Cmd} = get_cmd(ServiceInfo),
    create_command_list_and_update_state(Cmd, UserInfo, ServiceInfo,
                                         Connection, State).



connect_to_service(#{con_type := local}) ->
    {ok, local};
connect_to_service(#{con_type := ssh , con_host := Host } = Info ) ->
    Port = maps:get(con_port, Info, 22),
    User = maps:get(con_user, Info, "root"),
    UserDir = maps:get(con_ssh_user_dir, Info),
    ssh:connect(Host, Port,
                [{user_dir, UserDir}, {user_interaction, false},
                 {user, User}, {id_string, "TokenTranslationService"}]
                , 10000).

get_cmd(#{cmd := Cmd}) ->
    {ok, Cmd};
get_cmd(_) ->
    {error, no_cmd}.


create_command_list_and_update_state(Cmd, UserInfo, #{con_type := ConType},
                                     {ok, Connection}, State)
  when is_binary(Cmd) ->
    #{ uid := User,
       uidNumber := Uid,
       gidNumber := Gid,
       homeDirectory := HomeDir
     } = UserInfo,
    #state{
       action = Action,
       params = Params,
       cred_state = CredState
      } = State,
    EncodedJson = base64url:encode(jsx:encode(#{
                                     action => Action,
                                     user => User,
                                     uid => Uid,
                                     gid => Gid,
                                     home_dir => HomeDir,
                                     params => Params,
                                     cred_state => CredState
                                    })),
    CmdLine = << Cmd/binary, <<" ">>/binary, EncodedJson/binary >>,
    CmdList = [CmdLine],
    {ok, State#state{cmd_list=CmdList,
                     connection = Connection, con_type = ConType}}.

trigger_next_command() ->
    gen_server:cast(self(), execute_cmd).

execute_single_command_or_exit(State) ->
    #state{ cmd_list = CmdList,
            connection = Connection,
            error = Error,
            con_type = ConType,
            cmd_log = CmdLog
          } = State,
    execute_command_or_send_result(CmdList, ConType, Connection, Error,
                                   CmdLog, State).

execute_command_or_send_result([], ConType, Connection, undefined, Log
                               , State) ->
    close_connection(Connection, ConType),
    [LastLog|_] = Log,
    Result = create_result(LastLog, Log),
    {ok, NewState} = send_reply(Result, State),
    {stop, normal, NewState};
execute_command_or_send_result([Cmd|T], ConType, Connection, undefined, _Log
                               , State) ->
    {ok, NewState} = execute_command(Cmd, ConType, Connection, State),
    {noreply, NewState#state{ cmd_list = T}}.

create_result(#{exit_status := 0, std_out := []}, Log) ->
    {error, no_json, lists:reverse(Log)};
create_result(#{exit_status := 0, std_out := [Json|_]}, Log) ->
    case jsx:is_json(Json) of
        true -> {ok, jsx:decode(Json, [return_maps, {labels, attempt_atom}])
                 , lists:reverse(Log)};
        false -> {error, bad_json_result, lists:reverse(Log)}
    end;
create_result(#{exit_status := _}, Log) ->
    {error, script_failed, lists:reverse(Log) };
create_result(#{std_err := [], std_out := Data}, Log) ->
    create_result(#{std_out=>Data, exit_status => 0}, Log);
create_result(_, Log) ->
    create_result(#{exit_status => -1}, Log).


execute_command(Cmd, ssh, Connection, State) when is_list(Cmd) ->
    {ok, ChannelId} = ssh_connection:session_channel(Connection, infinity),
    success = ssh_connection:exec(Connection, ChannelId, Cmd, infinity),
    CmdState =  #{channel_id => ChannelId, cmd => Cmd},
    {ok, State#state{ cmd_state = CmdState }};
execute_command(Cmd, local, _Connection, #state{cmd_log=Log} = State)
  when is_list(Cmd) ->
    StdOut = list_to_binary(os:cmd(Cmd)),
    trigger_next_command(),
    CmdState = #{std_out => [StdOut], cmd => Cmd, std_err => []},
    NewState = State#state{ cmd_state = #{},
                            cmd_log = [ CmdState | Log] },
    {ok, NewState};
execute_command(Cmd, ConType, Connection, State)
  when is_binary(Cmd) ->
    execute_command(binary_to_list(Cmd), ConType, Connection, State).


handle_ssh_message({data, ChannelId, 0, Data}, ChannelId, State) ->
    % data on std out
    update_std_out_err(Data, <<>>, State);
handle_ssh_message({data, ChannelId, 1, Data}, ChannelId, State) ->
    % data on std err
    update_std_out_err(<<>>, Data, State);
handle_ssh_message({eof, ChannelId}, ChannelId, State) ->
    {ok, State};
handle_ssh_message({exit_signal, ChannelId, ExitSignal, ErrorMsg, Lang},
                   ChannelId, State) ->
    #state{ cmd_state = CmdState
          } = State,
    Update = #{exit_signale => ExitSignal, err_msg => ErrorMsg, lang => Lang},
    NewState = State#state{ cmd_state = maps:merge(CmdState, Update) },
    {ok, NewState};
handle_ssh_message({exit_status, ChannelId, ExitStatus}, ChannelId, State) ->
    #state{ cmd_state = CmdState
          } = State,
    Update = #{exit_status => ExitStatus},
    NewState = State#state{ cmd_state = maps:merge(CmdState, Update) },
    {ok, NewState};
handle_ssh_message({closed, ChannelId}, ChannelId, State) ->
    #state{ cmd_state = CmdState,
            cmd_log = Log
          } = State,
    trigger_next_command(),
    NewState = State#state{ cmd_state = #{},
                            cmd_log = [ CmdState | Log] },
    {ok, NewState}.

update_std_out_err(Out, Err, #state{ cmd_state = CmdState } = State) ->
    StdOut = [Out | maps:get(std_out, CmdState, [])],
    StdErr = [Err | maps:get(std_err, CmdState, [])],
    Update = #{std_out => StdOut, std_err => StdErr},
    NewState = State#state{ cmd_state = maps:merge(CmdState, Update) },
    {ok, NewState}.


close_connection(Connection, ssh) ->
    ok = ssh:close(Connection);
close_connection(_, local ) ->
    ok.

send_reply(_Reply, #state{client=undefined}=State) ->
    {ok, State};
send_reply(Reply, #state{client=Client}=State) ->
    gen_server:reply(Client, Reply),
    {ok, State#state{client=undefined}}.

