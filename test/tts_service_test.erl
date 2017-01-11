-module(tts_service_test).
-include_lib("eunit/include/eunit.hrl").

-define(ISSUER, <<"https://iam.it">>).
get_list_test() ->
    {ok, Meck} = start_meck(),
    {ok, List} = watts_service:get_list(),
    ?assertEqual(4, length(List)),
    ok = stop_meck(Meck),
    ok.


get_list_for_user_test() ->
    {ok, Meck} = start_meck(),
    {ok, Info0} = tts_userinfo:new(),
    {ok, Info} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, Info0),


    {ok, List} = watts_service:get_list(Info),
    ?assertEqual(3, length(List)),
    ok = stop_meck(Meck),
    ok.


get_service_info_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"id1">>,
    {ok, _} = watts_service:get_info(ServiceId),
    {error, not_found} = watts_service:get_info(some_other_service),
    ok = stop_meck(Meck),
    ok.

proxy_function_test() ->
    {ok, Meck} = start_meck(),
    Service1 = <<"id1">>,
    Service2 = <<"id2">>,
    Unknown = <<"idk">>,
    {ok, UserInfo0} = tts_userinfo:new(),
    {ok, UserInfo} = tts_userinfo:update_iss_sub(?ISSUER, <<"sub">>, UserInfo0),
    io:format("using userinfo ~p~n",[UserInfo]),

    ?assertEqual(true, watts_service:exists(Service1)),
    ?assertEqual(true, watts_service:is_allowed(UserInfo, Service1)),
    ?assertEqual({ok, 2}, watts_service:get_credential_limit(Service1)),
    ?assertEqual(true, watts_service:is_enabled(Service1)),
    ?assertEqual(false, watts_service:allows_same_state(Service1)),
    {ok, Queue1} = watts_service:get_queue(Service1),
    ?assertNotEqual(undefined, Queue1),

    ?assertEqual(true, watts_service:exists(Service2)),
    ?assertEqual(false, watts_service:is_allowed(UserInfo, Service2)),
    ?assertEqual({ok, 1}, watts_service:get_credential_limit(Service2)),
    ?assertEqual(false, watts_service:is_enabled(Service2)),
    ?assertEqual(true, watts_service:allows_same_state(Service2)),
    ?assertEqual({ok, undefined}, watts_service:get_queue(Service2)),

    ?assertEqual(false, watts_service:exists(Unknown)),
    ?assertEqual(false, watts_service:is_allowed(UserInfo, Unknown)),
    ?assertEqual({ok, 0}, watts_service:get_credential_limit(Unknown)),
    ?assertEqual(false, watts_service:is_enabled(Unknown)),
    ?assertEqual(false, watts_service:allows_same_state(Unknown)),
    ?assertEqual({ok, undefined}, watts_service:get_queue(Unknown)),

    ok = stop_meck(Meck),
    ok.

add_test() ->
    {ok, Meck} = start_meck(),
    BasicId = <<"basic_id">>,
    BasicService = #{id => BasicId},
    ?assertEqual({ok, BasicId}, watts_service:add(BasicService)),
    ?assertEqual({error, invalid_config}, watts_service:add(#{})),
    ok = stop_meck(Meck),
    ok.

params_test() ->
    {ok, Meck} = start_meck(),
    Params1 = <<"params1">>,
    Params2 = <<"params2">>,
    Unknown = <<"idk">>,
    ?assertEqual({error, not_found}, watts_service:update_params(Unknown)),
    ?assertEqual(ok, watts_service:update_params(Params1)),
    ?assertEqual(ok, watts_service:update_params(Params2)),
    ok = stop_meck(Meck),
    ok.



start_meck() ->
    MeckModules = [tts_data, tts_plugin, oidcc, jobs],
    ok = test_util:meck_new(MeckModules),
    ServiceList = [#{id => <<"id1">>,
                     authz => #{ allow => [{any, <<"sub">>, any, true}],
                                 forbid => [],
                                 tooltip => <<"tooltip">>,
                                 hide => false
                               },
                     cred_limit => 2,
                     enabled => true,
                     allow_same_state => false,
                     parallel_runner => 2,
                     queue => <<"ab">>
                    },
                   #{id => <<"id2">>,
                     authz => #{ allow => [{<<"other">>, <<"sub">>, any, true}],
                                 forbid => [],
                                 tooltip => <<"tooltip">>,
                                 hide => false
                               },
                     cred_limit => 1,
                     enabled => false,
                     allow_same_state => true,
                     parallel_runner => infinite,
                     queue => undefined
                    },
                   #{id => <<"params1">>,
                     authz => #{ allow => [{<<"iam">>, <<"sub">>, any, true}],
                                 forbid => [],
                                 tooltip => <<"tooltip">>,
                                 hide => false
                               },
                     cred_limit => 3,
                     plugin_conf_config => #{<<"boolean_test">> => <<"true">>},
                     cmd => <<"command">>,
                     parallel_runner => 2,
                     queue => <<"bc">>
                    },
                   #{id => <<"params2">>,
                     authz => #{ allow => [{<<"other">>, <<"sub">>, any, true}],
                                 forbid => [],
                                 tooltip => <<"tooltip">>,
                                 hide => true
                               },
                     cred_limit => 3,
                     plugin_conf_config => #{<<"boolean_test">> => <<"false">>},
                     cmd => <<"command">>,
                     parallel_runner => infinite
                    }],

    GetServiceList = fun() ->
                             {ok, ServiceList}
                     end,

    GetCredCount = fun(_UserInfo, _Service) ->
                           {ok, 1}
                   end,
    GetParams = fun(Id) ->
                        case Id of
                            <<"params1">> ->
                                {ok, #{conf_params => [#{name => <<"boolean_test">>,
                                                         type => <<"boolean">>,
                                                         default => <<"false">>
                                                        },
                                                       #{name => <<"string_test">>,
                                                         type => <<"string">>,
                                                         default => <<"false string">>
                                                        }
                                                      ],
                                       request_params => [
                                                          [
                                                           #{name => <<"a nice name">>,
                                                             key => <<"nice">>,
                                                             type => <<"textarea">>,
                                                             description => <<"a nice description">>
                                                            },
                                                           #{name => <<"a nicer name">>,
                                                             key => <<"nice2">>,
                                                             type => <<"textarea">>,
                                                             description => <<"other description">>,
                                                             mandatory => <<"true">>
                                                            }
                                                          ],[]],
                                       version => <<"devel">>}
                                };
                            <<"params2">> ->
                                {ok, #{conf_params => [#{name => <<"boolean_test">>,
                                                         type => <<"boolean">>,
                                                         default => <<"flse">>
                                                        }],
                                       request_params => [
                                                           #{name => <<"a nicer name">>,
                                                             key => <<"nice2">>,
                                                             description => <<"other description">>
                                                            }
                                                         ],
                                       version => <<"dev">>
                                      }
                                };
                            _ ->
                                {error, not_found}
                        end
                end,
    GetService = fun(SId) ->
                         Filter = fun(#{id := Id}) ->
                                          Id == SId
                                  end,
                         case lists:filter(Filter, ServiceList) of
                             [Service] ->
                                 {ok, {SId, Service}};
                             _ ->
                                 {error, not_found}
                         end
                 end,
    AddService = fun(_SId, _SInfo) ->
                         ok
                 end,
    ServiceUpdate = fun(Id, Info) ->
                            io:format("updating service ~p to ~p",[Id, Info]),
                            case Id of
                                <<"params1">> ->
                                    case maps:get(enabled, Info, false) of
                                        true ->
                                            ok;
                                        _ ->
                                            {error, not_enabled}
                                    end;
                                <<"params2">> ->
                                    case maps:get(enabled, Info, false) of
                                        false ->
                                            ok;
                                        _ ->
                                            {error, not_enabled}
                                    end;
                                _ ->
                                    ok
                            end
                    end,
    GetProviderList =
        fun() ->
                {ok, [{<<"iam">>, something}]}
        end,
    GetProviderInfo =
        fun(Id) ->
                case Id of
                    <<"iam">> ->
                        {ok, #{issuer => ?ISSUER, id => <<"iam">>}};
                    pid ->
                        {ok, #{issuer => ?ISSUER, id => <<"iam">>}};
                    _ ->
                        {error, not_found}
                end
        end,
    FindProvider = fun(Iss) ->
                           case Iss of
                               ?ISSUER ->
                                   {ok, pid};
                               _ ->
                                   {error, not_found}
                           end
                   end,

    AddQueue = fun(_, _) ->
                       ok
                   end,
    ok = meck:expect(tts_data, service_get, GetService),
    ok = meck:expect(tts_data, service_add, AddService),
    ok = meck:expect(tts_data, service_get_list, GetServiceList),
    ok = meck:expect(tts_data, service_update, ServiceUpdate),
    ok = meck:expect(tts_plugin, get_count, GetCredCount),
    ok = meck:expect(tts_plugin, get_params, GetParams),
    ok = meck:expect(oidcc, find_openid_provider, FindProvider),
    ok = meck:expect(oidcc, get_openid_provider_list, GetProviderList),
    ok = meck:expect(oidcc, get_openid_provider_info, GetProviderInfo),
    ok = meck:expect(jobs, add_queue, AddQueue),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
