-module(tts_init_test).
-include_lib("eunit/include/eunit.hrl").

basic_init_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_init:start_link(),
    application:set_env(tts, hostname, "localhost"),
    application:set_env(tts, ssl, false),
    application:set_env(tts, listen_port, 8080),
    application:set_env(tts, port, 8080),
    application:set_env(tts, redirection_enable, false),
    application:set_env(tts, redirection_port, 443),
    test_util:wait_for_process_to_die(Pid, 300),
    ok = stop_meck(Meck),
    ok.


advanced_init_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_init:start_link(),
    ServiceList = [#{}, #{}],
    ProviderList = [#{client_id => <<"1234">>,
                      client_secret => <<"seCret">>,
                      config_endpoint => <<"https://some.config">>,
                      description => <<"a provider">>,
                      id => <<"oidc1">>,
                      scopes => [<<"openid">>]}],
    application:set_env(tts, hostname, "localhost"),
    application:set_env(tts, ssl, true),
    application:set_env(tts, cert_file, "some_cert"),
    application:set_env(tts, key_file, "some_key"),
    application:set_env(tts, listen_port, 8080),
    application:set_env(tts, port, 8080),
    application:set_env(tts, redirection_enable, true),
    application:set_env(tts, redirection_port, 443),
    application:set_env(tts, service_list, ServiceList),
    application:set_env(tts, provider_list, ProviderList),
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
