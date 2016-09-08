-module(tts_service_test).
-include_lib("eunit/include/eunit.hrl").

get_list_test() ->
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, service_get_list, fun() -> {ok, []} end),
    {ok, []} = tts_service:get_list(),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    ok.


get_list_for_user_test() ->
    UserId = "joe",
    UserInfo = #{uid => UserId},
    UserInfoOidc = #{site => #{uid => UserId}},
    MeckModules = [tts_data, tts_credential],
    Id1 = <<"123">>,
    Id2 = <<"456">>,
    Id3 = <<"789">>,
    test_util:meck_new(MeckModules),

    GetServiceList = fun() ->
                             {ok, [#{id => Id1, cred_limit => 2},
                                   #{id => Id2, cred_limit => 1},
                                   #{id => Id3}
                                  ]}
                     end,
    GetCredCount = fun(_Uid, _Service) ->
                           {ok, 1}
                   end,
    ok = meck:expect(tts_data, service_get_list, GetServiceList),
    ok = meck:expect(tts_credential, get_count, GetCredCount),

    ExpResult = {ok, [
                      #{id => Id1,
                        limit_reached => false,
                        cred_limit => 2,
                        cred_count => 1
                       },
                      #{id => Id2,
                        limit_reached => true,
                        cred_limit => 1,
                        cred_count => 1
                       },
                      #{id => Id3,
                        limit_reached => true,
                        cred_count => 1
                       }
                     ]},
    ?assertEqual(ExpResult, tts_service:get_list(UserId)),
    ?assertEqual(ExpResult, tts_service:get_list(UserInfo)),
    ?assertEqual(ExpResult, tts_service:get_list(UserInfoOidc)),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    true = meck:validate(tts_credential),
    ok = meck:unload(tts_credential),
    ok.


get_service_info_test() ->
    ServiceId = <<"a very nice service">>,
    ok = meck:new(tts_data),
    ExpectServiceId = fun(Id) ->
                           case Id of
                               ServiceId ->
                                   {ok, {ServiceId, great_news}};
                               _ ->
                                   {error, not_found}
                           end
                   end,
    ok = meck:expect(tts_data, service_get, ExpectServiceId),
    {ok, great_news} = tts_service:get_info(ServiceId),
    {error, not_found} = tts_service:get_info(some_other_service),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    ok.

ssh_service_add_test() ->
    ServiceInfo1 = #{
      "Id" => "test ssh",
      "Host" => "ssh.example.com",
      "Port" => "22",
      "CredentialLimit" => "3",
      "RequestCmdFile" => "ssh_request.cmd",
      "RevokeCmdFile" => "ssh_revoke.cmd",
      "IncidentCmdFile" => "ssh_incident.cmd",
      "ConnectionType" => "ssh",
      "ConnectionUser" => "root",
      "ConnectionHost" => "ssh.example.com",
      "ConnectionPort" => "22",
      "ConnectionPassword" => "secret",
      "UnsupportedOption" => "123",
      "ConnectionSshKeyPass" => "1234",
      "ConnectionSshAutoAcceptHosts" => "false",
      "Cmd" => "~/bin/test.sh",
      "AllowSameState" => "true",
      "ConnectionSshDir" => "./.ssh/"},
    ServiceInfo2 = [],
    ok = meck:new(tts_data),
    ok = meck:new(erlydtl),
    ok = meck:expect(tts_data, service_add, fun(_, _) -> ok end),
    ok = meck:expect(erlydtl, compile_file, fun(_, _) -> {ok, some_data} end),
    ?assertEqual(ok, tts_service:add(ServiceInfo1)),
    ?assertEqual({error, invalid_config}, tts_service:add(ServiceInfo2)),
    true = meck:validate(tts_data),
    true = meck:validate(erlydtl),
    ok = meck:unload(tts_data),
    ok = meck:unload(erlydtl),
    ok.
