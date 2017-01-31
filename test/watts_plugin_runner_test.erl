-module(watts_plugin_runner_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

start_stop_test() ->
    {ok, Pid} = watts_plugin_runner:start_link(),
    ok = watts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = watts_plugin_runner:start_link(),
    ignored = gen_server:call(Pid, unsupported_call),
    ok = gen_server:cast(Pid, unsupported_cast),
    Pid ! unsupported_msg,
    ok = watts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

request_ssh_test() ->
    {ok, {SshPid,_} = Meck } = start_meck(),
    ServiceId = <<"ssh1">>,
    {ok, UserInfo0} = watts_userinfo:new(),
    {ok, UserInfo} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    Params = [],


    {ok, Pid} = watts_plugin_runner:start(),
    SshPid ! {pid, Pid},

    Config = #{action => request,
               service_id => ServiceId,
               user_info => UserInfo,
               params => Params,
               queue => undefined},
    {ok, #{credential:=_Credential, state:=_CredState},
     _} = watts_plugin_runner:request_action(Config, Pid),


    ok = watts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = stop_meck(Meck),
    ok.

request_local_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"local1">>,
    {ok, UserInfo0} = watts_userinfo:new(),
    {ok, UserInfo} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    Params = [],
    {ok, ReqPid} = watts_plugin_runner:start(),

    ConfigReq = #{action => request,
                  service_id => ServiceId,
                  user_info => UserInfo,
                  params => Params,
                  queue => undefined},
    {error, bad_json_result
     , _} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
    ok = test_util:wait_for_process_to_die(ReqPid,100),

    {ok, RevPid} = watts_plugin_runner:start(),
    ConfigRev = maps:merge(ConfigReq, #{action => revoke,
                                        cred_state => <<"credstate">>}),
    {error, bad_json_result
     , _} = watts_plugin_runner:request_action(ConfigRev, RevPid),
    ok = test_util:wait_for_process_to_die(RevPid,100),

    ok = stop_meck(Meck),
    ok.

no_cmd_crash_test() ->
    {ok, Meck} = start_meck(),

    ServiceId = <<"local_no_cmd">>,
    UserInfo = #{uid => <<"joe">>,
                 uidNumber => 1001,
                 gidNumber => 1001,
                 homeDirectory => <<"/home/joe">>
                },
    Params = [],

    Config = #{action => request,
               service_id => ServiceId,
               user_info => UserInfo,
               params => Params,
               queue => undefined},
    {ok, Pid} = watts_plugin_runner:start(),
    {error, {internal, _}} = watts_plugin_runner:request_action(Config, Pid),
    ok = watts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,1000),

    ok = stop_meck(Meck),
    ok.


start_meck() ->
    MeckModules = [watts_service, ssh, ssh_connection],
    ok = test_util:meck_new(MeckModules),
    ?SETCONFIG( vsn, "eunit"),
    SShCmd = <<"create credential">>,
    Credential = <<"secret">>,
    CredState = <<"internalState">>,
    JsonMap = #{ credential => Credential,
                 state => CredState
               },
    ServiceList = [
                   #{id => <<"ssh1">>,
                     connection => #{
                       host => <<"localhost">>,
                       port => 22,
                       passwd => undefined,
                       ssh_dir => <<"~/.ssh">>,
                       ssh_key_pass => undefined,
                       type => ssh,
                       user => <<"tts">>
                      },
                     cmd => SShCmd
                     },
                   #{id => <<"local_no_cmd">>,
                     connection => #{
                       host => undefined,
                       port => 22,
                       passwd => undefined,
                       ssh_dir => undefined,
                       ssh_key_pass => undefined,
                       type => local,
                       user => undefined
                      }
                   },
                   #{id => <<"local1">>,
                     cmd => <<"ls">>,
                     connection => #{
                       host => undefined,
                       port => 22,
                       passwd => undefined,
                       ssh_dir => undefined,
                       ssh_key_pass => undefined,
                       type => local,
                       user => undefined
                      }
                   }
                  ],
    InfoFun = fun(Id) ->
                      Filter = fun(#{id := SId}) ->
                                       SId == Id
                               end,
                      case lists:filter(Filter, ServiceList) of
                          [Info] ->
                              {ok, Info};
                          _ ->
                              {error, not_found}
                      end
              end,
    SshSend = fun() ->
                      Pid = receive
                                {pid, P} -> P
                            end,

                      receive
                          ssh_start ->
                              %% stdout
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 0, <<"working ...">>}},
                              %% stderr
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 1, <<"oops ...">>}},
                              %% no more data going to be sent
                              Json = jsone:encode(JsonMap),
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 0, Json}},
                              %%
                              Pid ! {ssh_cm, ssh, {eof, ssh_channel}},
                              %% exit signal ... what ever that is ...
                              Pid ! {ssh_cm, ssh, {exit_signal, ssh_channel, "", "", ""}},
                              %% everything went well
                              Pid ! {ssh_cm, ssh, {exit_status, ssh_channel, 0}},
                              %% done
                              Pid ! {ssh_cm, ssh, {closed, ssh_channel}};
                          stop ->
                              ok
                      end
              end,

    SshPid = spawn(SshSend),
    ConnectFun = fun(H, P, _Options, _Timeout) ->
                         H = <<"localhost">>,
                         P = 22,
                         {ok, ssh}
                 end,
    CloseFun = fun(Con) ->
                       Con = ssh,
                       ok
               end,
    ChannelFun = fun(Con, _Timeout) ->
                         Con = ssh,
                         {ok, ssh_channel}
                 end,

    ExecFun = fun(Con, Chan, InCmd, _Timeout) ->
                         Con = ssh,
                         Chan = ssh_channel,
                         InCmdBin = binary:list_to_bin(InCmd),
                         SShCmd =  binary:part(InCmdBin, 0,
                                            byte_size(SShCmd)),
                        SshPid ! ssh_start,
                        success
                 end,
    ok = meck:expect(watts_service,get_info,InfoFun),
    ok = meck:expect(ssh,connect,ConnectFun),
    ok = meck:expect(ssh,close,CloseFun),
    ok = meck:expect(ssh_connection,session_channel,ChannelFun),
    ok = meck:expect(ssh_connection,exec,ExecFun),
    {ok, {SshPid, MeckModules}}.


stop_meck({SshPid, MeckModules}) ->
    SshPid ! stop,
    ok = test_util:meck_done(MeckModules),
    ?UNSETCONFIG( vsn),
    ok.
