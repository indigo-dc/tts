-module(tts_plugin_runner_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_plugin_runner:start_link(),
    ok = tts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = tts_plugin_runner:start_link(),
    ignored = gen_server:call(Pid, unsupported_call),
    ok = gen_server:cast(Pid, unsupported_cast),
    Pid ! unsupported_msg,
    ok = tts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

request_ssh_test() ->
    {ok, {SshPid,_} = Meck } = start_meck(),
    ServiceId = <<"ssh1">>,
    {ok, UserInfo0} = tts_userinfo:new(),
    {ok, UserInfo} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    Params = [],


    {ok, Pid} = tts_plugin_runner:start(),
    SshPid ! {pid, Pid},

    {ok, #{credential:=_Credential, state:=_CredState},
     _} = tts_plugin_runner:request(ServiceId, UserInfo,Params,undefined,Pid),


    ok = tts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = stop_meck(Meck),
    ok.

request_local_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"local1">>,
    {ok, UserInfo0} = tts_userinfo:new(),
    {ok, UserInfo} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    Params = [],
    {ok, ReqPid} = tts_plugin_runner:start(),

    {error, bad_json_result
     , _} = tts_plugin_runner:request(ServiceId, UserInfo,Params, undefined, ReqPid),
    ok = test_util:wait_for_process_to_die(ReqPid,100),

    {ok, RevPid} = tts_plugin_runner:start(),
    {error, bad_json_result
     , _} = tts_plugin_runner:revoke(ServiceId, UserInfo,<<"credstate">>, undefined, RevPid),
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

    {ok, Pid} = tts_plugin_runner:start(),
    {error, {internal, _}} = tts_plugin_runner:request(ServiceId, UserInfo,Params, undefined,Pid),
    ok = tts_plugin_runner:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,1000),

    ok = stop_meck(Meck),
    ok.


start_meck() ->
    MeckModules = [tts_service, ssh, ssh_connection],
    ok = test_util:meck_new(MeckModules),
    application:set_env(tts, vsn, "eunit"),
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
    ok = meck:expect(tts_service,get_info,InfoFun),
    ok = meck:expect(ssh,connect,ConnectFun),
    ok = meck:expect(ssh,close,CloseFun),
    ok = meck:expect(ssh_connection,session_channel,ChannelFun),
    ok = meck:expect(ssh_connection,exec,ExecFun),
    {ok, {SshPid, MeckModules}}.


stop_meck({SshPid, MeckModules}) ->
    SshPid ! stop,
    ok = test_util:meck_done(MeckModules),
    application:unset_env(tts, vsn),
    ok.
