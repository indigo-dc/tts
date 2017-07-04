-module(watts_init_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

basic_init_test() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG( hostname, "localhost"),
        ?SETCONFIG( ssl, false),
        ?SETCONFIG( listen_port, 8080),
        ?SETCONFIG( port, 8080),
        ?SETCONFIG( redirection_enable, false),
        ?SETCONFIG( redirection_port, 443),
        ?SETCONFIG( persistent_module, watts_data_sqlite),
        {ok, Pid} = watts_init:start_link(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.


advanced_init_test() ->
    {ok, Meck} = start_meck(),
    try
        ServiceList = [#{id => <<"info">>}, #{id => <<"ssh">>}],
        ProviderList = [#{client_id => <<"1234">>,
                          client_secret => <<"seCret">>,
                          config_endpoint => <<"https://some.config">>,
                          description => <<"a provider">>,
                          id => <<"oidc1">>,
                          scopes => [<<"openid">>],
                          disable_login => false
                         }],
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
        ?SETCONFIG( persistent_module, watts_data_sqlite),
        {ok, Pid} = watts_init:start_link(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

start_meck() ->
    MeckModules = [watts_data_sqlite, cowboy, oidcc, oidcc_client,
                   watts_service],
    Initialize = fun() ->
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
    AddProvider = fun(_, _, _) ->
                          {ok, id, pid}
                  end,
    GetProviderList = fun() ->
                          {ok, [{id1, pid1}, {id2, pid2}]}
                  end,
    GetProviderConf = fun(Pid) ->
                              case Pid of
                                  pid1 -> {ok, #{ready => true}};
                                  _ -> {ok, #{ready => false}}
                              end
                      end,
    GetProviderError = fun(Pid) ->
                               case Pid of
                                   pid1 -> {ok, undefined};
                                   _ -> {ok, {error, something}}
                               end
                       end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts_data_sqlite, initialize, Initialize),
    ok = meck:expect(watts_data_sqlite, is_ready, Initialize),
    ok = meck:expect(watts_service, add, AddService),
    ok = meck:expect(watts_service, update_params, UpdateParams),
    ok = meck:expect(oidcc_client, register, RegisterClient),
    ok = meck:expect(oidcc, add_openid_provider, AddProvider),
    ok = meck:expect(oidcc, get_openid_provider_list, GetProviderList),
    ok = meck:expect(oidcc_openid_provider, get_config, GetProviderConf),
    ok = meck:expect(oidcc_openid_provider, get_error, GetProviderError),
    ok = meck:expect(cowboy, start_http, StartHttp),
    ok = meck:expect(cowboy, start_https, StartHttp),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
