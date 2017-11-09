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
    {ok, {SshPid,_, _} = Meck } = start_meck(),
    try
        ServiceId = <<"ssh1">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
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
        ok = test_util:wait_for_process_to_die(Pid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

request_local_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local1">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, ReqPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, ReqPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},


        {error, bad_json
        , _} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
        ok = test_util:wait_for_process_to_die(ReqPid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

revoke_local_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local1">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, RevPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, RevPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},

        ConfigRev = maps:merge(ConfigReq, #{action => revoke,
                                            cred_state => <<"credstate">>}),
        {error, bad_json
        , _} = watts_plugin_runner:request_action(ConfigRev, RevPid),
        ok = test_util:wait_for_process_to_die(RevPid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

request_env_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local2">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, ReqPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, ReqPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},
        {ok, ReqMap, _} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
        ?assertEqual(<<"ok">>, maps:get(result, ReqMap, undefined)),
        ok = test_util:wait_for_process_to_die(ReqPid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

revoke_env_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local2">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, RevPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, RevPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},
        ConfigRev = maps:merge(ConfigReq, #{action => revoke,
                                            cred_state => <<"credstate">>}),
        {ok, RevMap, _} = watts_plugin_runner:request_action(ConfigRev, RevPid),
        ?assertEqual(<<"ok">>, maps:get(result, RevMap, undefined)),
        ok = test_util:wait_for_process_to_die(RevPid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

request_timeout_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local3">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, ReqPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, ReqPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},

        {error, timeout} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
        ok = test_util:wait_for_process_to_die(ReqPid,100),
        ok
    after
        ok = stop_meck(Meck)
    end,
    ok.

request_parts_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local4">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        Params = [],
        {ok, ReqPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, ReqPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      params => Params,
                      queue => undefined},

        {ok, Map, Output} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
        io:format("output: ~p",[Output]),
        ok = test_util:wait_for_process_to_die(ReqPid,100),
        ?assertEqual(<<"ok">>, maps:get(result, Map, undefined))
    after
        ok = stop_meck(Meck)
    end,
    ok.

plugin_api_test() ->
    {ok, {_, ExecPid, _} = Meck} = start_meck(),
    try
        ServiceId = <<"local5">>,
        {ok, UserInfo0} = watts_userinfo:new(),
        {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
        {ok, UserInfo} = watts_userinfo:update_access_token(#{token => <<"at">>}, UserInfo1),
        {ok, ReqPid} = watts_plugin_runner:start(),
        ExecPid ! {pid, ReqPid},

        ConfigReq = #{action => request,
                      service_id => ServiceId,
                      user_info => UserInfo,
                      queue => undefined},

        {ok, Cred, _} = watts_plugin_runner:request_action(ConfigReq, ReqPid),
        ok = test_util:wait_for_process_to_die(ReqPid,100),
        %% the Cred contains the passed parameter to the plugin as state
        State = maps:get(state, Cred),
        Map = jsone:decode(base64url:decode(State), [{keys, attempt_atom}, {object_format, map}]),
        io:format("map passed to plugin: ~p~n", [Map]),
        ?assertEqual(true, is_map(Map)),
        ?assertEqual(true, is_binary(maps:get(watts_version, Map))),
        ?assertEqual(true, is_binary(maps:get(action, Map))),
        ?assertEqual(true, is_map(maps:get(conf_params, Map))),
        ?assertEqual(true, is_map(maps:get(params, Map))),
        ?assertEqual(true, is_map(maps:get(user_info, Map))),
        UsrInfo = maps:get(user_info, Map),
        ?assertEqual(true, is_binary(maps:get(iss, UsrInfo))),
        ?assertEqual(true, is_binary(maps:get(sub, UsrInfo)))
    after
        ok = stop_meck(Meck)
    end,
    ok.


no_cmd_crash_test() ->
    {ok, Meck} = start_meck(),
    try
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
        ok = test_util:wait_for_process_to_die(Pid,1000)
    after
        ok = stop_meck(Meck)
    end,
    ok.


start_meck() ->
    MeckModules = [watts_service, ssh, ssh_connection, exec],
    ok = test_util:meck_new(MeckModules),
    ?SETCONFIG( vsn, "eunit"),
    SShCmd = <<"create credential">>,
    Credential = <<"secret">>,
    CredState = <<"internalState">>,
    JsonMap = #{ credential => Credential,
                 state => CredState,
                 result => ok
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
                   },
                   #{id => <<"local2">>,
                     cmd => <<"ls">>,
                     cmd_env_use => true,
                     cmd_env_var => "WATTS_PARAMETER",
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
                   #{id => <<"local3">>,
                     plugin_timeout => 500,
                     cmd => <<"timeout">>,
                     cmd_env_use => true,
                     cmd_env_var => "WATTS_PARAMETER",
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
                   #{id => <<"local4">>,
                     plugin_timeout => 500,
                     cmd => <<"parts">>,
                     cmd_env_use => true,
                     cmd_env_var => "WATTS_PARAMETER",
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
                   #{id => <<"local5">>,
                     plugin_timeout => 500,
                     cmd => <<"env">>,
                     cmd_env_use => true,
                     cmd_env_var => "WATTS_PARAMETER",
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
                              Json = jsone:encode(JsonMap),
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 0, Json}},
                              %% stdout
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 0, <<"working ...">>}},
                              %% stderr
                              Pid ! {ssh_cm, ssh, {data, ssh_channel, 1, <<"oops ...">>}},
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

    ExecSend = fun() ->
                      Pid = receive
                                {pid, P} -> P
                            end,

                      receive
                          exec_good_start ->
                              %% stdout
                              Json = jsone:encode(JsonMap),
                              Pid ! {stdout, 123,  Json},
                              %% everything went well
                              Pid ! {'DOWN', 123, process, pid1, normal};
                          exec_parts_start ->
                              Pid ! {stdout, 345,  <<"{ \"result\": \"ok\",">>},
                              Pid ! {stdout, 345,  <<"\"state\": \"test\",">>},
                              Pid ! {stdout, 345,  <<"\"credential\": []}">>},
                              Pid ! {'DOWN', 345, process, pid3, normal};
                          {exec_return_input, Input} ->
                              InputMap = #{ credential => [],
                                           state => Input,
                                           result => ok
                                         },
                              Pid ! {stdout, 456,  jsone:encode(InputMap)},
                              %% everything went well
                              Pid ! {'DOWN', 456, process, pid4, normal};
                          exec_bad_start ->
                              %% stdout
                              Pid ! {stdout, 999,  <<"working ...">>},
                              %% stderr
                              Pid ! {stderr, 999, <<"oops ...">>},
                              %% everything went well
                              Pid ! {'DOWN', 999, process, pid9, normal};
                          stop ->
                              ok
                      end
              end,
    ExecPid = spawn(ExecSend),
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
    ErlexecRun = fun(Cmd, Params) ->
                         Env = lists:keyfind(env, 1, Params),
                         case Cmd of
                             "ls" ->
                                 ExecPid ! exec_good_start,
                                 {ok, pid1, 123};
                             "timeout" ->
                                 {ok, pid2, 234};
                             "parts" ->
                                 ExecPid ! exec_parts_start,
                                 {ok, pid3, 345};
                             "env" ->
                                 {env, [{_, Data}]} = Env,
                                 ExecPid ! {exec_return_input, list_to_binary(Data)},
                                 {ok, pid4, 456};
                             _ ->
                                 ExecPid ! exec_bad_start,
                                 {ok, pid9, 999}
                         end

                 end,

    ok = meck:expect(watts_service, get_info ,InfoFun),
    ok = meck:expect(ssh, connect ,ConnectFun),
    ok = meck:expect(ssh, close ,CloseFun),
    ok = meck:expect(ssh_connection, session_channel, ChannelFun),
    ok = meck:expect(ssh_connection, exec, ExecFun),
    ok = meck:expect(exec, run, ErlexecRun),
    ok = meck:expect(exec, stop, fun(_) -> ok end),
    ok = meck:expect(exec, status, fun(_) -> {status, 0} end),

    {ok, {SshPid, ExecPid, MeckModules}}.





stop_meck({SshPid, ExecPid, MeckModules}) ->
    SshPid ! stop,
    ExecPid ! stop,
    ok = test_util:meck_done(MeckModules),
    ?UNSETCONFIG( vsn),
    ok.
