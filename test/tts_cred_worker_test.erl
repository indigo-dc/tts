-module(tts_cred_worker_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_cred_worker:start_link(),
    ok = tts_cred_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = tts_cred_worker:start_link(),
    ignored = gen_server:call(Pid, unsupported_call),
    ok = gen_server:cast(Pid, unsupported_cast),
    Pid ! unsupported_msg,
    ok = tts_cred_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

request_ssh_test() ->
    MeckModules = [tts_service, ssh, ssh_connection],

    ServiceId = <<"ssh1">>,
    Cmd = <<"create credential">>,
    Host = <<"localhost">>,
    ServiceInfo = #{con_type => ssh,
                    con_host => Host,
                    con_ssh_user_dir => <<"~/.ssh">>,
                   cmd => Cmd
                   },
    UserInfo = #{uid => <<"joe">>,
                 uidNumber => 1001,
                 gidNumber => 1001,
                 homeDirectory => <<"/home/joe">>
                },
    Params = [],
    Credential = <<"secret">>,
    CredState = <<"internalState">>,
    JsonMap = #{ credential => Credential,
                 state => CredState
               },
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
                              Json = jsx:encode(JsonMap),
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 0, Json}},
                              %%
                              Pid ! {ssh_cm, ssh, {eof, ssh_channel}},
                              %% exit signal ... what ever that is ...
                              Pid ! {ssh_cm, ssh, {exit_signal, ssh_channel, "", "", ""}},
                              %% everything went well
                              Pid ! {ssh_cm, ssh, {exit_status, ssh_channel, 0}},
                              %% done
                              Pid ! {ssh_cm, ssh, {closed, ssh_channel}}
                      end
              end,

    SshPid = spawn(SshSend),
    InfoFun = fun(Id) ->
                      Id = ServiceId,
                      {ok, ServiceInfo}
              end,
    ConnectFun = fun(H, P, _Options, _Timeout) ->
                         H = Host,
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
                         Cmd =  binary:part(InCmdBin, 0,
                                            byte_size(Cmd)),

                        SshPid ! ssh_start,
                        success
                 end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_service,get_info,InfoFun),
    ok = meck:expect(ssh,connect,ConnectFun),
    ok = meck:expect(ssh,close,CloseFun),
    ok = meck:expect(ssh_connection,session_channel,ChannelFun),
    ok = meck:expect(ssh_connection,exec,ExecFun),


    {ok, Pid} = tts_cred_worker:start(),
    SshPid ! {pid, Pid},

    {ok, #{credential:=Credential, state:=CredState},
     _} = tts_cred_worker:request(ServiceId, UserInfo,Params,Pid),


    ok = tts_cred_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

request_local_test() ->
    MeckModules = [tts_service],

    ServiceId = <<"local1">>,
    Cmd = <<"ls">>,
    ServiceInfo = #{con_type => local,
                   cmd => Cmd
                   },
    UserInfo = #{uid => <<"joe">>,
                 uidNumber => 1001,
                 gidNumber => 1001,
                 homeDirectory => <<"/home/joe">>
                },
    Params = [],
    InfoFun = fun(Id) ->
                      Id = ServiceId,
                      {ok, ServiceInfo}
              end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_service,get_info,InfoFun),

    {ok, Pid} = tts_cred_worker:start(),

    {error, bad_json_result
     , _} = tts_cred_worker:request(ServiceId, UserInfo,Params,Pid),


    ok = tts_cred_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

no_cmd_crah_test() ->
    MeckModules = [tts_service],

    ServiceId = <<"local1">>,
    ServiceInfo = #{con_type => local
                   },
    UserInfo = #{uid => <<"joe">>,
                 uidNumber => 1001,
                 gidNumber => 1001,
                 homeDirectory => <<"/home/joe">>
                },
    Params = [],
    InfoFun = fun(Id) ->
                      Id = ServiceId,
                      {ok, ServiceInfo}
              end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_service,get_info,InfoFun),

    {ok, Pid} = tts_cred_worker:start(),

    {error, {internal, _}} = tts_cred_worker:request(ServiceId, UserInfo,Params,Pid),


    ok = tts_cred_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

