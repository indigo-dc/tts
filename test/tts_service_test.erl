-module(tts_service_test).
-include_lib("eunit/include/eunit.hrl").

get_list_test() ->
    {ok, Meck} = start_meck(),
    {ok, List} = tts_service:get_list(),
    ?assertEqual(4, length(List)),
    ok = stop_meck(Meck),
    ok.


get_list_for_user_test() ->
    {ok, Meck} = start_meck(),
    {ok, Info0} = tts_userinfo:new(),
    {ok, Info} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, Info0),


    {ok, List} = tts_service:get_list(Info),
    ?assertEqual(3, length(List)),
    ok = stop_meck(Meck),
    ok.


get_service_info_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"id1">>,
    {ok, _} = tts_service:get_info(ServiceId),
    {error, not_found} = tts_service:get_info(some_other_service),
    ok = stop_meck(Meck),
    ok.

proxy_function_test() ->
    {ok, Meck} = start_meck(),
    Service1 = <<"id1">>,
    Service2 = <<"id2">>,
    Unknown = <<"idk">>,
    UserInfo = #{},

    ?assertEqual(true, tts_service:exists(Service1)),
    ?assertEqual(true, tts_service:is_allowed(UserInfo, Service1)),
    ?assertEqual({ok, 2}, tts_service:get_credential_limit(Service1)),
    ?assertEqual(true, tts_service:is_enabled(Service1)),
    ?assertEqual(false, tts_service:allows_same_state(Service1)),
    {ok, Queue1} = tts_service:get_queue(Service1),
    ?assertNotEqual(undefined, Queue1),

    ?assertEqual(true, tts_service:exists(Service2)),
    ?assertEqual(false, tts_service:is_allowed(UserInfo, Service2)),
    ?assertEqual({ok, 1}, tts_service:get_credential_limit(Service2)),
    ?assertEqual(false, tts_service:is_enabled(Service2)),
    ?assertEqual(true, tts_service:allows_same_state(Service2)),
    ?assertEqual({ok, undefined}, tts_service:get_queue(Service2)),

    ?assertEqual(false, tts_service:exists(Unknown)),
    ?assertEqual(false, tts_service:is_allowed(UserInfo, Unknown)),
    ?assertEqual({ok, 0}, tts_service:get_credential_limit(Unknown)),
    ?assertEqual(false, tts_service:is_enabled(Unknown)),
    ?assertEqual(false, tts_service:allows_same_state(Unknown)),
    ?assertEqual({ok, undefined}, tts_service:get_queue(Unknown)),

    ok = stop_meck(Meck),
    ok.



%% ssh_service_add_test() ->
%%     {ok, Meck} = start_meck(),
%%     Service1 = #{id => <<"id42">>},
%%     Service2 = #{},
%%     ?assertEqual(ok, tts_service:add(Service1)),
%%     ?assertEqual({error, invalid_config}, tts_service:add(Service2)),
%%     ok = stop_meck(Meck),
%%     ok.

start_meck() ->
    MeckModules = [tts_data, tts_plugin, tts_service_authz],
    ok = test_util:meck_new(MeckModules),
    Service1 = #{id => <<"id1">>,
                 authz => #{ allow => [{any, <<"sub">>, any, true}],
                             forbid => [],
                             tooltip => <<"tooltip">>,
                             hide => false
                           },
                 cred_limit => 2,
                 enabled => true,
                 allow_same_state => false,
                 parallel_runner => 2
                },
    Service2 = #{id => <<"id2">>,
                 authz => #{ allow => [{<<"other">>, <<"sub">>, any, true}],
                             forbid => [],
                             tooltip => <<"tooltip">>,
                             hide => false
                           },
                 cred_limit => 1,
                 enabled => false,
                 allow_same_state => true,
                 parallel_runner => infinite
                },
    Service3 = #{id => <<"id3">>,
                 authz => #{ allow => [{<<"iss">>, <<"sub">>, any, true}],
                             forbid => [],
                             tooltip => <<"tooltip">>,
                             hide => false
                           },
                 cred_limit => 3
                },
    ServiceHidden = #{id => <<"hidden_id">>,
                 authz => #{ allow => [{<<"other">>, <<"sub">>, any, true}],
                             forbid => [],
                             tooltip => <<"tooltip">>,
                             hide => true
                           },
                 cred_limit => 3
                },

    GetServiceList = fun() ->
                             {ok, [Service1, Service2, Service3, ServiceHidden]}
                     end,

    GetCredCount = fun(_UserInfo, _Service) ->
                           {ok, 1}
                   end,
    GetService = fun(SId) ->
                         case SId of
                             <<"id1">> -> {ok, {SId, Service1}};
                             <<"id2">> -> {ok, {SId, Service2}};
                             <<"id3">> -> {ok, {SId, Service3}};
                             <<"hidden_id">> -> {ok, {SId, ServiceHidden}};
                             _ ->
                                 {error, not_found}
                         end
                 end,
    ServiceUpdate = fun(_, _) ->
                            ok
                    end,
    IsAllowed = fun(Id, _, _) ->
                        case Id of
                            <<"id1">> -> true;
                            _ -> false
                        end
                end,
    ok = meck:expect(tts_data, service_get, GetService),
    ok = meck:expect(tts_data, service_get_list, GetServiceList),
    ok = meck:expect(tts_data, service_update, ServiceUpdate),
    ok = meck:expect(tts_plugin, get_count, GetCredCount),
    ok = meck:expect(tts_service_authz, is_authorized, IsAllowed),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
