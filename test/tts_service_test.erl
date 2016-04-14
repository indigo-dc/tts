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
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, service_get_list, fun() -> {ok, []} end),

    ok = meck:new(tts_credential),
    ExpectUserId = fun(Uid) ->
                           case Uid of
                               UserId ->
                                   {ok, []};
                               _ ->
                                   {error, wrong_user}
                           end
                   end,
    ok = meck:expect(tts_credential, get_list, ExpectUserId),
    {ok, []} = tts_service:get_list(UserId),
    {ok, []} = tts_service:get_list(UserInfo),
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
    ServiceInfo = #{
      "Id" => "test ssh",
      "Host" => "ssh.example.com",
      "Port" => "22",
      "RequestCmdFile" => "ssh_request.cmd",
      "RevokeCmdFile" => "ssh_revoke.cmd",
      "IncidentCmdFile" => "ssh_incident.cmd",
      "ConnectionType" => "ssh",
      "ConnectionUser" => "root",
      "ConnectionHost" => "ssh.example.com",
      "ConnectionPort" => "22",
      "UnsupportedOption" => "123",
      "ConnectionSshDir" => "./.ssh/"},
    ok = meck:new(tts_data),
    ok = meck:new(erlydtl),
    ok = meck:expect(tts_data, service_add, fun(_, _) -> ok end),
    ok = meck:expect(erlydtl, compile_file, fun(_, _) -> {ok, some_data} end),
    ok = tts_service:add(ServiceInfo),
    true = meck:validate(tts_data),
    true = meck:validate(erlydtl),
    ok = meck:unload(tts_data),
    ok = meck:unload(erlydtl),
    ok.



