-module(tts_init_test).
-include_lib("eunit/include/eunit.hrl").
-include("tts.hrl").

basic_init_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_init:start_link(),
    ?SETCONFIG( hostname, "localhost"),
    ?SETCONFIG( ssl, false),
    ?SETCONFIG( listen_port, 8080),
    ?SETCONFIG( port, 8080),
    ?SETCONFIG( redirection_enable, false),
    ?SETCONFIG( redirection_port, 443),
    test_util:wait_for_process_to_die(Pid, 300),
    ok = stop_meck(Meck),
    ok.


advanced_init_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_init:start_link(),
    ServiceList = [#{id => <<"info">>}, #{id => <<"ssh">>}],
    ProviderList = [#{client_id => <<"1234">>,
                      client_secret => <<"seCret">>,
                      config_endpoint => <<"https://some.config">>,
                      description => <<"a provider">>,
                      id => <<"oidc1">>,
                      scopes => [<<"openid">>]}],
    ?SETCONFIG( hostname, "localhost"),
    ?SETCONFIG( ssl, true),
    ?SETCONFIG( cert_file, "some_cert"),
    ?SETCONFIG( key_file, "some_key"),
    ?SETCONFIG( listen_port, 8080),
    ?SETCONFIG( port, 8080),
    ?SETCONFIG( redirection_enable, true),
    ?SETCONFIG( redirection_port, 443),
    ?SETCONFIG( service_list, ServiceList),
    ?SETCONFIG( provider_list, ProviderList),
    test_util:wait_for_process_to_die(Pid, 300),
    ok = stop_meck(Meck),
    ok.

start_meck() ->
    MeckModules = [tts_data_sqlite, cowboy, oidcc, oidcc_client, tts_service],
    Reconfigure = fun() ->
                          ok
                  end,
    AddService = fun(_) ->
                         {ok, id}
                  end,
    UpdateParams = fun(_) ->
                         ok
                  end,
    RegisterClient = fun(_) ->
                             ok
                     end,
    StartHttp = fun(_, _, _ ,_) ->
                        {ok, id}
                end,
    AddProvider = fun(_, _, _ ,_ ,_ ,_ , _, _) ->
                          {ok, id, pid}
                  end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_data_sqlite, reconfigure, Reconfigure),
    ok = meck:expect(tts_data_sqlite, is_ready, Reconfigure),
    ok = meck:expect(tts_service, add, AddService),
    ok = meck:expect(tts_service, update_params, UpdateParams),
    ok = meck:expect(oidcc_client, register, RegisterClient),
    ok = meck:expect(oidcc, add_openid_provider, AddProvider),
    ok = meck:expect(cowboy, start_http, StartHttp),
    ok = meck:expect(cowboy, start_https, StartHttp),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
