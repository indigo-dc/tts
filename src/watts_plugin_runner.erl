%% @doc this module takes care of running a plugin and controlling and
%% validating its results.
%% It is implemented as a gen_server so that each run of a plugin has a
%% corresponding process in the VM.
-module(watts_plugin_runner).
%%
%% Copyright 2016 - 2017 SCC/KIT
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

-include("watts.hrl").

%% API.
-export([start_link/0]).
-export([start/0]).
-export([stop/1]).
-export([request_action/2]).

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
          service_info = undefined :: undefined | watts_service:info(),
          user_info = undefined,
          params = undefined,
          queue = undefined,
          job_id = undefined,
          cred_state = undefined,
          connection = undefined,
          con_type = undefined,
          cmd_line = undefined,
          cmd_stdin = undefined :: undefined | binary(),
          cmd_output = undefined :: undefined | output(),
          error = undefined
         }).

-export_type([config/0, result/0, output/0]).

-type state() :: #state{}.

-type config() :: #{action =>  parameter | request | revoke ,
                    service_id => binary(),
                    queue => atom(),
                    user_info => watts_userinfo:userinfo() | undefined,
                    cred_state => binary() | undefined,
                    params => map()
                   }.

-type result() ::
        {ok, Result :: map(), Output :: output()} |
        {error, Reason::atom() | tuple(), Output :: output()} |
        {error, Reason::atom() | tuple()}.

-type output() :: #{ channel_id => any(),
                     process_id => any(),
                     cmd => string() | undefined,
                     std_out => [binary()],
                     std_err => [binary()],
                     exit_status => any()
                   }.
%% API.


%% @doc start the gen_server process in a linked manner.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, noparams, []).

%% @doc start the gen_server process (not linked).
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start(?MODULE, noparams, []).

%% @doc stop the gen_server.
-spec stop(Pid::pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


%% @doc Request the action configured in the config passed.
%% The action can be one of the suppoerted actions:
%% <ul>
%% <li> parameter </li>
%% <li> request </li>
%% <li> revoke </li>
%% </ul>
-spec request_action(config(), pid()) -> result().
request_action(#{service_id := ServiceId} = ConfigIn, Pid) ->
    try
        {ok, ServiceInfo} = watts_service:get_info(ServiceId),
        Timeout = maps:get(plugin_timeout, ServiceInfo, infinity),
        Config = maps:put(service_info, ServiceInfo, ConfigIn),
        gen_server:call(Pid, {request_action,  Config}, Timeout)
    catch
        exit:{timeout, _} ->
            gen_server:cast(Pid, kill),
            timeout_result()
    end.


%% gen_server.
%% @doc init the gen_server process, this just returns an empty state.
-spec init(noparams) -> {ok, state()}.
init(noparams) ->
    {ok, #state{}}.


%% @doc hande the only call, the action request.
%% This will start the process to iterate throught all the needed steps.
-spec handle_call({request_action, config()} | any(), any(), state()) ->
                         {noreply, state(), pos_integer()} |
                         {reply, ignored, state()}.
handle_call({request_action,
             #{action := Action, service_info := ServiceInfo} = Config},
             From, #state{client = undefined} = State ) ->
    UserInfo = maps:get(user_info, Config, undefined),
    CredState = maps:get(cred_state, Config, undefined),
    Params = maps:get(params, Config, #{}),
    Queue = maps:get(queue, Config, undefined),
    NewState = State#state{ action = Action,
                            client = From,
                            service_info = ServiceInfo,
                            user_info = UserInfo,
                            cred_state = CredState,
                            params = Params,
                            queue = Queue
                          },
    gen_server:cast(self(), request_slot),
    {noreply, NewState, 200};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%% @doc each step in the process of executing the plugin is done here.
%% The actions performed are:
%% <ul>
%% <li> request a slot in the queue </li>
%% <li> run the plugin (perform action) </li>
%% <li> return the result </li>
%% </ul>
-spec handle_cast(any(), state()) ->
                         {noreply, state()} |
                         {stop, normal, state()}.
handle_cast(request_slot, #state{action = parameter}=State) ->
    gen_server:cast(self(), run_plugin),
    {noreply, State};
handle_cast(request_slot, #state{queue = undefined}=State) ->
    gen_server:cast(self(), run_plugin),
    {noreply, State};
handle_cast(request_slot, #state{queue = Queue}=State) ->
    case jobs:ask(Queue) of
        {ok, JobId} ->
            gen_server:cast(self(), run_plugin),
            {noreply, State#state{job_id=JobId}};
        {error, rejected} ->
            {ok, NewState} = send_reply({error, rejected}, State),
            {stop, normal, NewState}
    end;
handle_cast(run_plugin, State) ->
    {ok, NewState} = run_plugin(State),
    {noreply, NewState};
handle_cast(send_result, State) ->
    send_result(State);
handle_cast(kill, State) ->
    kill(State);
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc handle the responses from either ssh or the executed plugin.
%% @see handle_exec_message/3
%% @see handle_ssh_message/3
-spec handle_info(any(), state()) -> {noreply, state()} |
                                     {stop, normal, state()}.
handle_info({ssh_cm, SshCon, SshMsg}
            , #state{connection = SshCon, con_type = ssh,
                     cmd_output = #{ channel_id := ChannelId}}=State) ->
    {ok, NewState} = handle_ssh_message(SshMsg, ChannelId, State),
    {noreply, NewState};
handle_info({Type, ProcessId, Msg}
            , #state{ con_type = local,
                 cmd_output = #{ process_id := ProcessId}}=State) ->
    {ok, NewState} = handle_exec_message(Type, Msg, State),
    {noreply, NewState};
handle_info({'DOWN', ProcessId, process, _, Reason}
            , #state{ con_type = local,
                 cmd_output = #{ process_id := ProcessId}}=State) ->
    {ok, NewState} = handle_exec_message(down, Reason, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc check if crashed and send a reply then, if not do nothing.
-spec terminate(any(), state()) -> ok.
terminate(Reason, State) ->
    lager:debug("runner ~p: terminating", [self()]),
    {ok, _NewState} = send_reply({error, {internal, Reason}}, State),
    ok.

%% @doc only returns the current state.
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc prepare and run a plugin
-spec run_plugin(state()) -> {ok, state()}.
run_plugin(State) ->
    {ok, NewState} = prepare_action(State),
    #state{ cmd_line = Cmd,
            cmd_stdin = Stdin,
            connection = Connection,
            con_type = ConType
          } = NewState,
    run_plugin(Cmd, Stdin, ConType, Connection, NewState).


%% @doc prepare the execution by creating the command line and connecting.
-spec prepare_action(state()) -> {ok, state()}.
prepare_action(State) ->
    #state{service_info = ServiceInfo,
           user_info = UserInfo
          } = State,
    {ok, ConnInfo} = get_connection_info(ServiceInfo),
    {ok, Plugin} = get_plugin(ServiceInfo),
    Connection = connect_to_service(ConnInfo),
    create_command_and_update_state(Plugin, UserInfo, ServiceInfo,
                                         Connection, State).


%% @doc connect to the plugin, either locally or via ssh.
-spec connect_to_service(watts_service:connection()) -> {ok, any()}.
connect_to_service(#{type := local}) ->
    lager:debug("runner ~p: using local connection", [self()]),
    {ok, local};
connect_to_service(#{type := ssh , host := Host } = Info ) ->
    Port = maps:get(port, Info),
    User = maps:get(user, Info),
    AcceptHosts = false,
    Options0 = [
                {id_string, "TokenTranslationService"},
                {user, User},
                {user_interaction, false},
                {silently_accept_hosts, AcceptHosts}
               ],
    OptionsMapping = [
                      {ssh_dir, user_dir},
                      {passwd, password},
                      {ssh_key_pass, rsa_pass_phrase}
                     ],
    AddOption = fun({MapKey, OptionKey}, {Inf, Options}) ->
                        case maps:get(MapKey, Inf, undefined) of
                            undefined -> {Inf, Options};
                            Value -> {Inf, [{OptionKey, Value} | Options]}
                        end
                end,
    {_, Options} = lists:foldl(AddOption, {Info, Options0}, OptionsMapping),
    lager:debug("runner ~p: connecting to ~p:~p using ssh with options ~p",
                [self(), Host, Port, Options]),
    ssh:connect(Host, Port, Options, 10000).


%% @doc get the plugin
-spec get_plugin(watts_service:info()) -> {ok, binary()}.
get_plugin(#{cmd := Cmd}) ->
    {ok, Cmd}.

%% @doc return the connection info from the service info
-spec get_connection_info(watts_service:info()) ->
                                 {ok, watts_service:connection()}.
get_connection_info(#{connection := ConnInfo}) ->
    {ok, ConnInfo}.

%% @doc create the command line command and update the state with it.
-spec create_command_and_update_state(binary(), watts_userinfo:info(),
                                      watts_service:info(), {ok, any()},
                                      state()) -> {ok, state()}.
create_command_and_update_state(Cmd, UserInfo, ServiceInfo,
                                {ok, Connection}, State)
  when is_binary(Cmd) ->
    #state{
       action = Action,
       user_info = UserInfo,
       cred_state = CredState
      } = State,
    ServiceId = maps:get(id, ServiceInfo),
    ConnInfo = maps:get(connection, ServiceInfo, #{}),
    AddAccessToken = maps:get(pass_access_token, ServiceInfo, false),
    ConnType = maps:get(type, ConnInfo, local),
    {ok, Version} = ?CONFIG_(vsn),
    BasicParameter = #{
                        watts_version => list_to_binary(Version),
                        action => Action,
                        cred_state => CredState
                      },
    ParamUpdate = parameter_update(Action, ServiceInfo, State),
    ScriptParam0 = maps:merge( BasicParameter, ParamUpdate),
    ScriptParam = add_user_info_if_present(ScriptParam0, UserInfo,
                                           ServiceId, AddAccessToken),
    EncodedJson = base64url:encode(jsone:encode(ScriptParam)),
    Features = maps:get(plugin_features, ServiceInfo),
    lager:debug("runner ~p: will execute ~p with parameter ~p",
                [self(), Cmd, ScriptParam]),
    UseStdin = case maps:get(stdin, Features, false) of
                   true -> true;
                   _    -> false
               end,
    {CmdLine, CmdStdin} = create_cmd_and_stdin(Cmd, EncodedJson, UseStdin),
    % marcus: this is useful for logging!!
    lager:debug("runner ~p: the command line is (parameter in base64url): ~p. CmdStdin: ~p",
                [self(), CmdLine, CmdStdin]),
    {ok, State#state{cmd_line=CmdLine, cmd_stdin = CmdStdin,
                     connection = Connection, con_type = ConnType}}.


%% @doc return the config and request parameter, if not performing parameter.
-spec parameter_update(atom(), watts_service:info(), state()) -> map().
parameter_update(parameter, _ServiceInfo, _State) ->
    #{};
parameter_update(_, ServiceInfo, #state{ params = Params }) ->
    ConfParams = maps:get(plugin_conf, ServiceInfo, #{}),
    #{conf_params => ConfParams,
      params => Params}.


%% @doc create the command to execute and the stdin input
-spec create_cmd_and_stdin(binary(), binary(), boolean())
                        -> {string(), undefined | binary()}.
create_cmd_and_stdin(Cmd, EncJson, false) ->
    {binary_to_list(<< Cmd/binary, <<" ">>/binary, EncJson/binary >>),
     undefined};
create_cmd_and_stdin(Cmd, EncJson, true) ->
    {binary_to_list(Cmd), EncJson}.


%% @doc add the information about the user to the parameter
-spec add_user_info_if_present(map(), watts_userinfo:info() | undefined,
                               binary(), boolean()) -> map().
add_user_info_if_present(ScriptParam, undefined, _, _) ->
    ScriptParam;
add_user_info_if_present(ScriptParam, UserInfo, ServiceId, AddAccessToken) ->
    {ok, AccessToken} = watts_userinfo:return(access_token, UserInfo),
    {ok, UserId} = watts_userinfo:return(id, UserInfo),
    {ok, PluginUserInfo} = watts_userinfo:return(plugin_info, UserInfo),
    {ok, AdditionalInfo} = watts_userinfo:return({additional_logins, ServiceId,
                                                  AddAccessToken}, UserInfo),
    BaseUpdate = #{watts_userid => UserId, user_info => PluginUserInfo,
                  additional_logins => AdditionalInfo},
    Update = case AddAccessToken of
                 true ->
                     maps:merge(BaseUpdate, #{access_token => AccessToken});
                 false ->
                     BaseUpdate
             end,
    maps:merge(ScriptParam, Update).

%% @doc run the plugin with its parameter.
%% The results will be sent async to this process and are handled by the
%% {@link handle_info/2} function.
-spec run_plugin(string(), binary()| undefined, ssh | local, any(), state()) ->
                             {ok, state()}.
run_plugin(Cmd, Stdin, ssh, Connection, State) ->
    {ok, ChannelId} = ssh_connection:session_channel(Connection, infinity),
    lager:debug("runner ~p: executing ~p", [self(), Cmd]),
    success = ssh_connection:exec(Connection, ChannelId, Cmd, infinity),
    ok = maybe_ssh_stdin_send(Connection, ChannelId, Stdin),
    CmdOutput =  #{channel_id => ChannelId, cmd => Cmd},
    {ok, State#state{ cmd_output = output(CmdOutput) }};
run_plugin(Cmd, Stdin, local, _Connection, State) ->
    lager:debug("runner ~p: executing ~p", [self(), Cmd]),
    {ok, _Pid, Id} = exec:run(Cmd, [stdin, stdout, stderr, monitor,
                                    {kill_timeout, 1}]),
    ok = maybe_local_stdin_send(Id, Stdin),
    CmdOutput =  #{process_id => Id, cmd => Cmd},
    NewState = State#state{ cmd_output = output(CmdOutput)},
    {ok, NewState}.

%% send the stdin data, if needed, else do nothing
-spec maybe_ssh_stdin_send(any(), any(), undefined | binary()) -> ok.
maybe_ssh_stdin_send(_, _, undefined) ->
    ok;
maybe_ssh_stdin_send(Connection, ChannelId, Stdin) ->
    ok = ssh_connection:send(Connection, ChannelId, Stdin),
    ok = ssh_connection:send_eof(Connection, ChannelId),
    ok.

%% send the stdin data, if needed, else do nothing
-spec maybe_local_stdin_send(any(), undefined | binary()) -> ok.
maybe_local_stdin_send(_, undefined) ->
    ok;
maybe_local_stdin_send(Id, Stdin) ->
    ok = exec:send(Id, Stdin),
    ok = exec:send(Id, eof),
    ok.


%% @doc send the result to the requesting process and stop this one.
-spec send_result(state()) -> {stop, normal, state()}.
send_result(#state{ cmd_output = CmdOutput, connection = Connection,
                    con_type = ConType} = State) ->
    lager:debug("runner ~p: execution done, closing connection", [self()]),
    close_connection(Connection, ConType),
    Result = create_result(CmdOutput),
    lager:debug("runner ~p: sending result ~p", [self(), Result]),
    {ok, NewState} = send_reply(Result, State),
    {stop, normal, NewState}.


%% @doc kill this process and the execution.
%% this is called after a timeout.
kill(#state{con_type = local,
            cmd_output = #{process_id := ProcessId}} = State) ->
    ok = exec:stop(ProcessId),
    {stop, normal, State#state{client = undefined}};
kill(#state{con_type = ssh, connection = Connection} = State) ->
    close_connection(Connection, ssh),
    {stop, normal, State#state{client = undefined}}.


%% @doc create the result from the command output.
-spec create_result(output()) -> result().
create_result(#{std_out := StdOut} = Output) ->
    Prepend = fun(Bin, Result) ->
                     << Bin/binary, Result/binary>>
             end,
    Json = lists:foldl(Prepend, <<>>, StdOut),
    case jsone:try_decode(Json, [{keys, attempt_atom}, {object_format, map}]) of
        {ok, Map, _} when is_map(Map) -> {ok, Map, Output};
        {ok, Other, _} -> {error, {no_map, Other}, Output};
        {error, _} ->
            {error, bad_json, Output}
    end.


%% @doc handle the messages from the exec module, when done send the result.
-spec handle_exec_message(atom(), binary(), state()) -> {ok, state()}.
handle_exec_message(stdout, Data, State) ->
    % data on std out
    update_std_out_err(Data, <<>>, State);
handle_exec_message(stderr, Data, State) ->
    % data on std err
    update_std_out_err(<<>>, Data, State);
handle_exec_message(down, {status, Status}, State) ->
    ExitStatus = case exec:status(Status) of
                     {status, Num} -> Num;
                     {signal, _, _} -> -999
                 end,
    update_exit_and_send(ExitStatus, State);
handle_exec_message(down, normal, State) ->
    update_exit_and_send(0, State);
handle_exec_message(down, ExitStatus, State) ->
    update_exit_and_send(ExitStatus, State).


%% @doc handle the incomming ssh messages and when done trigger sending result.
-spec handle_ssh_message(tuple(), any(), state()) -> {ok, state()}.
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
    #state{ cmd_output = CmdOutput
          } = State,
    Update = #{exit_signale => ExitSignal, err_msg => ErrorMsg, lang => Lang},
    NewState = State#state{cmd_output = output(maps:merge(CmdOutput, Update))},
    {ok, NewState};
handle_ssh_message({exit_status, ChannelId, ExitStatus}, ChannelId, State) ->
    update_exit_status(ExitStatus, State);
handle_ssh_message({closed, ChannelId}, ChannelId, State) ->
    #state{ cmd_output = CmdOutput} = State,
    lager:debug("runner ~p: result ~p", [self(), CmdOutput]),
    trigger_sending(),
    {ok, State}.


%% @doc update the std out or error list of inputs.
-spec update_std_out_err(binary(), binary(), state()) -> {ok, state()}.
update_std_out_err(Out, Err, #state{ cmd_output = CmdOutput } = State) ->
    StdOut = [Out | maps:get(std_out, CmdOutput, [])],
    StdErr = [Err | maps:get(std_err, CmdOutput, [])],
    Update = #{std_out => StdOut, std_err => StdErr},
    NewState = State#state{ cmd_output = maps:merge(CmdOutput, Update) },
    {ok, NewState}.


%% @doc update the exit status of the command and trigger sending the result
-spec update_exit_and_send(any(), state()) -> {ok, state()}.
update_exit_and_send(ExitStatus, State) ->
    Result = update_exit_status(ExitStatus, State),
    trigger_sending(),
    Result.

%% @doc update the exit status in the cmd output
-spec update_exit_status(any(), state()) -> {ok, state()}.
update_exit_status(ExitStatus, #state{ cmd_output = CmdOutput} = State) ->
    Update = #{exit_status => ExitStatus},
    NewState = State#state{ cmd_output = maps:merge(CmdOutput, Update) },
    {ok, NewState}.

%% @doc close the connection
-spec close_connection(any(), ssh | local) -> ok.
close_connection(Connection, ssh) ->
    ok = ssh:close(Connection);
close_connection(_, local ) ->
    ok.

%% @doc send the reply if not yet done.
-spec send_reply(result(), state()) -> {ok, state()}.
send_reply(_Reply, #state{client=undefined}=State) ->
    {ok, State};
send_reply(Reply, #state{client=Client}=State) ->
    gen_server:reply(Client, Reply),
    {ok, State#state{client=undefined}}.

%% @doc trigger sending the result.
-spec trigger_sending() -> ok.
trigger_sending() ->
    gen_server:cast(self(), send_result).

%% @doc generate a timout result
-spec timeout_result() -> result().
timeout_result() ->
    {error, timeout}.

%% @doc create a valid basic output
-spec output(map()) -> output().
output(Output) ->
    BasicOutput = #{channel_id => undefined,
                    process_id => undefined,
                    cmd => undefined,
                    std_out => [],
                    std_err => [],
                    exit_status => undefined},
    maps:merge(BasicOutput, Output).
