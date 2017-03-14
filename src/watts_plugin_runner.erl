-module(watts_plugin_runner).
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

-include("watts.hrl").

%% API.
-define(TIMEOUT, 20000).

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
          service_info = undefined,
          user_info = undefined,
          params = undefined,
          queue = undefined,
          job_id = undefined,
          cred_state = undefined,
          connection = undefined,
          con_type = undefined,
          cmd_line = undefined,
          cmd_output = undefined,
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


request_action(#{action := Action, service_id := ServiceId} = ConfigIn, Pid) ->
    try
        {ok, ServiceInfo} = watts_service:get_info(ServiceId),
        Timeout = maps:get(plugin_timeout, ServiceInfo, infinity),
        Config = maps:put(service_info, ServiceInfo, ConfigIn),
        gen_server:call(Pid, {request_action,  Config}, Timeout)
    catch
        exit:{timeout, _} ->
            timeout_result(Action)
    end.


%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({request_action,
             #{action := Action, service_info := ServiceInfo} = Config},
             From, #state{client = undefined} = State ) ->
    UserInfo = maps:get(user_info, Config, undefined),
    CredState = maps:get(cred_state, Config, undefined),
    Params = maps:get(params, Config, undefined),
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



handle_cast(request_slot, #state{action = parameter}=State) ->
    gen_server:cast(self(), perform_action),
    {noreply, State};
handle_cast(request_slot, #state{queue = undefined}=State) ->
    gen_server:cast(self(), perform_action),
    {noreply, State};
handle_cast(request_slot, #state{queue = Queue}=State) ->
    case jobs:ask(Queue) of
        {ok, JobId} ->
            gen_server:cast(self(), perform_action),
            {noreply, State#state{job_id=JobId}};
        {error, rejected} ->
            {ok, NewState} = send_reply({error, rejected}, State),
            {stop, normal, NewState}
    end;
handle_cast(perform_action, State) ->
    {ok, NewState} = execute_command(State),
    {noreply, NewState};
handle_cast(send_result, State) ->
    send_result(State);
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ssh_cm, SshCon, SshMsg}
            , #state{connection=SshCon,
                     cmd_output = #{ channel_id := ChannelId}}=State) ->
    {ok, NewState} = handle_ssh_message(SshMsg, ChannelId, State),
    {noreply, NewState};
handle_info(timeout, State) ->
    {ok, NewState} = send_reply({error, timeout}, State),
    {stop, normal, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    lager:debug("runner ~p: terminating", [self()]),
    {ok, _NewState} = send_reply({error, {internal, Reason}}, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



execute_command(State) ->
    {ok, NewState} = prepare_action(State),
    #state{ cmd_line = Cmd,
            connection = Connection,
            con_type = ConType
          } = NewState,
    execute_command(Cmd, ConType, Connection, NewState).


prepare_action(State) ->
    #state{service_info = ServiceInfo,
           user_info = UserInfo
          } = State,
    {ok, ConnInfo} = get_connection_info(ServiceInfo),
    {ok, Cmd} = get_cmd(ServiceInfo),
    Connection = connect_to_service(ConnInfo),
    create_command_and_update_state(Cmd, UserInfo, ServiceInfo,
                                         Connection, State).



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

get_cmd(#{cmd := Cmd}) ->
    {ok, Cmd};
get_cmd(_) ->
    {error, no_cmd}.

get_connection_info(#{connection := ConnInfo}) ->
    {ok, ConnInfo};
get_connection_info(_) ->
    {error, no_connection_info}.


create_command_and_update_state(Cmd, UserInfo, ServiceInfo,
                                {ok, Connection}, State)
  when is_binary(Cmd) ->
    #state{
       action = Action,
       params = Params,
       user_info = UserInfo,
       cred_state = CredState
      } = State,
    ConfParams = maps:get(plugin_conf, ServiceInfo, #{}),
    ServiceId = maps:get(id, ServiceInfo),
    ConnInfo = maps:get(connection, ServiceInfo, #{}),
    AddAccessToken = maps:get(pass_access_token, ServiceInfo, false),
    ConnType = maps:get(type, ConnInfo, local),
    {ok, Version} = ?CONFIG_(vsn),
    ParamUpdate =
        case Action == parameter of
            false ->
                #{conf_params => ConfParams,
                  params => Params};
            _ -> #{}
        end,
    ScriptParam0 = maps:merge(
                     #{
                        watts_version => list_to_binary(Version),
                        action => Action,
                        cred_state => CredState
                      }, ParamUpdate),
    ScriptParam = add_user_info_if_present(ScriptParam0, UserInfo,
                                           ServiceId, AddAccessToken),
    EncodedJson = base64url:encode(jsone:encode(ScriptParam)),
    lager:debug("runner ~p: will execute ~p with parameter ~p",
                [self(), Cmd, ScriptParam]),
    CmdLine = << Cmd/binary, <<" ">>/binary, EncodedJson/binary >>,
    lager:debug("runner ~p: the command line is (parameter in base64url): ~p",
                [self(), CmdLine]),
    {ok, State#state{cmd_line=CmdLine,
                     connection = Connection, con_type = ConnType}}.

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

execute_command(Cmd, ssh, Connection, State) when is_list(Cmd) ->
    {ok, ChannelId} = ssh_connection:session_channel(Connection, infinity),
    lager:debug("runner ~p: executing ~p", [self(), Cmd]),
    success = ssh_connection:exec(Connection, ChannelId, Cmd, infinity),
    CmdOutput =  #{channel_id => ChannelId, cmd => Cmd},
    {ok, State#state{ cmd_output = CmdOutput }};
execute_command(Cmd, local, _Connection, State) when is_list(Cmd) ->
    lager:debug("runner ~p: executing ~p", [self(), Cmd]),
    StdOut = list_to_binary(os:cmd(Cmd)),
    lager:debug("runner ~p: result ~p", [self(), StdOut]),
    CmdOutput = #{std_out => [StdOut], cmd => Cmd, std_err => []},
    NewState = State#state{ cmd_output = CmdOutput },
    trigger_sending(),
    {ok, NewState};
execute_command(Cmd, ConType, Connection, State)
  when is_binary(Cmd) ->
    execute_command(binary_to_list(Cmd), ConType, Connection, State).



send_result(#state{ cmd_output = CmdOutput, connection = Connection,
                    con_type = ConType} = State) ->
    lager:debug("runner ~p: execution done, closing connection", [self()]),
    close_connection(Connection, ConType),
    Result = create_result(CmdOutput),
    lager:debug("runner ~p: sending result ~p", [self(), Result]),
    {ok, NewState} = send_reply(Result, State),
    {stop, normal, NewState}.

create_result(#{exit_status := 0, std_out := []} = Output) ->
    {error, no_json, Output};
create_result(#{exit_status := 0, std_out := [Json|_]} = Output) ->
    case jsone:try_decode(Json, [{keys, attempt_atom}, {object_format, map}]) of
        {ok, Map, _} -> {ok, Map, Output};
        {error, _} -> {error, bad_json_result, Output}
    end;
create_result(#{exit_status := _} = Output) ->
    {error, script_failed, Output};
create_result(#{std_err := []} = Output) ->
    create_result(maps:put(exit_status, 0, Output));
create_result(Output) ->
    create_result(maps:put(exit_status, -1, Output)).




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
    NewState = State#state{ cmd_output = maps:merge(CmdOutput, Update) },
    {ok, NewState};
handle_ssh_message({exit_status, ChannelId, ExitStatus}, ChannelId, State) ->
    #state{ cmd_output = CmdOutput
          } = State,
    Update = #{exit_status => ExitStatus},
    NewState = State#state{ cmd_output = maps:merge(CmdOutput, Update) },
    {ok, NewState};
handle_ssh_message({closed, ChannelId}, ChannelId, State) ->
    #state{ cmd_output = CmdOutput} = State,
    lager:debug("runner ~p: result ~p", [self(), CmdOutput]),
    trigger_sending(),
    {ok, State}.

update_std_out_err(Out, Err, #state{ cmd_output = CmdOutput } = State) ->
    StdOut = [Out | maps:get(std_out, CmdOutput, [])],
    StdErr = [Err | maps:get(std_err, CmdOutput, [])],
    Update = #{std_out => StdOut, std_err => StdErr},
    NewState = State#state{ cmd_output = maps:merge(CmdOutput, Update) },
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

trigger_sending() ->
    gen_server:cast(self(), send_result).


timeout_result(Action) ->
    UMsg = "the plugin timed out, please inform the administrator",
    LMsg = io_lib:format("~s timed out", [atom_to_list(Action)]),
    {ok, #{result => error, user_msg => UMsg, log_msg => LMsg}, []}.
