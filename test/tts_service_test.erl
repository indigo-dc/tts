-module(tts_service_test).
-include_lib("eunit/include/eunit.hrl").

get_list_test() ->
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, service_get_list, fun() -> {ok, []} end),
    {ok, []} = tts_service:get_list(),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    ok.


%% get_list_for_user_test() ->
%%     UserId = "joe",
%%     UserInfo = #{uid => UserId},
%%     UserInfoOidc = #{site => #{uid => UserId}},
%%     MeckModules = [tts_data, tts_credential],
%%     Id1 = <<"123">>,
%%     Id2 = <<"456">>,
%%     Id3 = <<"789">>,
%%     test_util:meck_new(MeckModules),

%%     GetServiceList = fun() ->
%%                              {ok, [#{id => Id1, cred_limit => 2},
%%                                    #{id => Id2, cred_limit => 1},
%%                                    #{id => Id3}
%%                                   ]}
%%                      end,
%%     GetCredCount = fun(_Uid, _Service) ->
%%                            {ok, 1}
%%                    end,
%%     ok = meck:expect(tts_data, service_get_list, GetServiceList),
%%     ok = meck:expect(tts_credential, get_count, GetCredCount),

%%     ExpResult = {ok, [
%%                       #{id => Id1,
%%                         limit_reached => false,
%%                         cred_limit => 2,
%%                         cred_count => 1
%%                        },
%%                       #{id => Id2,
%%                         limit_reached => true,
%%                         cred_limit => 1,
%%                         cred_count => 1
%%                        },
%%                       #{id => Id3,
%%                         limit_reached => true,
%%                         cred_count => 1
%%                        }
%%                      ]},
%%     ?assertEqual(ExpResult, tts_service:get_list(UserId)),
%%     ?assertEqual(ExpResult, tts_service:get_list(UserInfo)),
%%     ?assertEqual(ExpResult, tts_service:get_list(UserInfoOidc)),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.


%% get_service_info_test() ->
%%     ServiceId = <<"a very nice service">>,
%%     ok = meck:new(tts_data),
%%     ExpectServiceId = fun(Id) ->
%%                            case Id of
%%                                ServiceId ->
%%                                    {ok, {ServiceId, great_news}};
%%                                _ ->
%%                                    {error, not_found}
%%                            end
%%                    end,
%%     ok = meck:expect(tts_data, service_get, ExpectServiceId),
%%     {ok, great_news} = tts_service:get_info(ServiceId),
%%     {error, not_found} = tts_service:get_info(some_other_service),
%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data),
%%     ok.

%% proxy_function_test() ->
%%     Service1 = id1,
%%     Service2 = id2,
%%     Service3 = id3,
%%     MeckModules = [tts_data],
%%     test_util:meck_new(MeckModules),
%%     GetService = fun(SId) ->
%%                          case SId of
%%                              Service1 ->
%%                                  {ok, {id1, #{cred_limit => 2,
%%                                               enabled => true,
%%                                               allow_same_state => false
%%                                              }}};
%%                              Service2 ->
%%                                  {ok, {id1, #{cred_limit => 1,
%%                                               enabled => false,
%%                                               allow_same_state => true
%%                                              }}};
%%                              Service3 ->
%%                                  {error, not_found}
%%                          end
%%                  end,
%%     ServiceUpdate = fun(_Id, _NewMap) -> ok end,
%%     ok = meck:expect(tts_data, service_get, GetService),
%%     ok = meck:expect(tts_data, service_update, ServiceUpdate),

%%     ?assertEqual({ok, 2}, tts_service:get_credential_limit(Service1)),
%%     ?assertEqual(true, tts_service:is_enabled(Service1)),
%%     ?assertEqual(false, tts_service:allows_same_state(Service1)),
%%     ?assertEqual(ok, tts_service:enable(Service1)),
%%     ?assertEqual(ok, tts_service:disable(Service1)),

%%     ?assertEqual({ok, 1}, tts_service:get_credential_limit(Service2)),
%%     ?assertEqual(false, tts_service:is_enabled(Service2)),
%%     ?assertEqual(true, tts_service:allows_same_state(Service2)),
%%     ?assertEqual(ok, tts_service:enable(Service2)),
%%     ?assertEqual(ok, tts_service:disable(Service2)),

%%     ?assertEqual({ok, 0}, tts_service:get_credential_limit(Service3)),
%%     ?assertEqual(false, tts_service:is_enabled(Service3)),
%%     ?assertEqual(false, tts_service:allows_same_state(Service3)),
%%     ?assertEqual({error, not_found}, tts_service:enable(Service3)),
%%     ?assertEqual({error, not_found}, tts_service:disable(Service3)),

%%     ok = test_util:meck_done(MeckModules),
%%     ok.



%% ssh_service_add_test() ->
%%     ServiceInfo1 = #{
%%       "Id" => "test ssh",
%%       "Host" => "ssh.example.com",
%%       "Port" => "22",
%%       "CredentialLimit" => "3",
%%       "RequestCmdFile" => "ssh_request.cmd",
%%       "RevokeCmdFile" => "ssh_revoke.cmd",
%%       "IncidentCmdFile" => "ssh_incident.cmd",
%%       "ConnectionType" => "ssh",
%%       "ConnectionUser" => "root",
%%       "ConnectionHost" => "ssh.example.com",
%%       "ConnectionPort" => "22",
%%       "ConnectionPassword" => "secret",
%%       "UnsupportedOption" => "123",
%%       "ConnectionSshKeyPass" => "1234",
%%       "ConnectionSshAutoAcceptHosts" => "false",
%%       "Cmd" => "~/bin/test.sh",
%%       "AllowSameState" => "true",
%%       "ConnectionSshDir" => "./.ssh/"},
%%     ServiceInfo2 = [],
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, service_add, fun(_, _) -> ok end),
%%     ?assertEqual(ok, tts_service:add(ServiceInfo1)),
%%     ?assertEqual({error, invalid_config}, tts_service:add(ServiceInfo2)),
%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data),
%%     ok.
