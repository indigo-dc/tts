-module(watts_plugin_test).
-include_lib("eunit/include/eunit.hrl").
-include("tts.hrl").

start_stop_test() ->
    {ok, Pid} = watts_plugin:start_link(),
    ok = watts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = watts_plugin:start_link(),
    ignored = gen_server:call(Pid,unsupported_call),
    ok = gen_server:cast(Pid,unsupported_cast),
    Pid ! unsupported_msg,
    ok = watts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

get_list_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"ssh1">>,
    OtherId = <<"ssh2">>,
    CredId1 = <<"some credential id1">>,
    OtherCred = <<"cred2">>,

    {ok, UserInfo0} = watts_userinfo:new(),
    {ok, UserInfo} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    {ok, Pid} = watts_plugin:start_link(),

    ?assertEqual({ok, [#{service_id => ServiceId}]},
                 watts_plugin:get_cred_list(UserInfo)),
    ?assertEqual({ok, 1}, watts_plugin:get_count(UserInfo, ServiceId)),
    ?assertEqual({ok, 0}, watts_plugin:get_count(UserInfo, OtherId)),
    ?assertEqual(true, watts_plugin:exists(UserInfo, CredId1)),
    ?assertEqual(false, watts_plugin:exists(UserInfo, OtherCred)),
    ok = watts_plugin:stop(),
    test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.

request_test() ->
    Service1 = <<"ssh1">>,
    Service2 = <<"ssh2">>,
    Service3 = <<"ssh3">>,
    Service4 = <<"ssh4">>,
    Service5 = <<"ssh5">>,
    Service6 = <<"ssh6">>,
    Cred1 = #{name => "password", type => <<"text">>, value => <<"secret">>},
    Cred2 = #{name => "secret1", type => <<"textarea">>, value => <<"secret">>},
    Cred3 = #{name => "secret2", type => <<"textarea">>, value => <<"#{id => <<\"secret\">>}">>},
    Cred4 = #{name => "secret3", type => <<"textfile">>, value => <<"secret">>},
    Cred5 = #{name => "secret4", type => <<"textfile">>, value => <<"secretlines">>},
    Interface = <<"test interface">>,
    Params = [],

    {ok, Meck} = start_meck(),

    {ok, UserInfo0} = watts_userinfo:new(),
    {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    {ok, UserInfo2} = watts_userinfo:update_iss_sub(<<"iss">>, <<"su">>, UserInfo0),
    {ok, UserInfo3} = watts_userinfo:update_iss_sub(<<"other">>, <<"sub">>, UserInfo0),

    {ok, Pid} = watts_plugin:start_link(),
    ?assertEqual({ok, #{id => <<"123">>, entries => [Cred1]}},
        watts_plugin:request(Service1, UserInfo1, Interface, Params)),

    ?assertEqual({ok, #{id => <<"123">>, entries => [Cred2]}},
        watts_plugin:request(Service2, UserInfo1, Interface, Params)),

    ?assertEqual({ok, #{id => <<"123">>, entries => [Cred3]}},
        watts_plugin:request(Service3, UserInfo1, Interface, Params)),

    ?assertEqual({ok, #{id => <<"123">>, entries => [Cred4]}},
        watts_plugin:request(Service4, UserInfo1, Interface, Params)),

    ?assertEqual({ok, #{id => <<"123">>, entries => [Cred5]}},
        watts_plugin:request(Service5, UserInfo1, Interface, Params)),

    ?assertEqual({error,#{log_msg => <<"logged">>,user_msg => <<"some message">>}},
                 watts_plugin:request(Service1, UserInfo2, Interface, Params)),

    ?assertEqual({error, limit_reached},
                 watts_plugin:request(Service2, UserInfo2, Interface, Params)),

    {error, #{user_msg := _, log_msg := _}} =
                 watts_plugin:request(Service3, UserInfo2, Interface, Params),

    {error, #{user_msg := _, log_msg := _}} =
                 watts_plugin:request(Service4, UserInfo2, Interface, Params),

    {error, #{user_msg := _, log_msg := _}} =
                 watts_plugin:request(Service5, UserInfo2, Interface, Params),

    ?assertEqual({error, service_disabled},
                 watts_plugin:request(Service6, UserInfo2, Interface, Params)),


    ?assertEqual({error, user_not_allowed},
                 watts_plugin:request(Service1, UserInfo3, Interface, Params)),


    ok = watts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),

    ok = stop_meck(Meck),
    ok.

revoke_test() ->
    {ok, Meck} = start_meck(),
    {ok, UserInfo0} = watts_userinfo:new(),
    {ok, UserInfo1} = watts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    CredId1 = <<"some credential id1">>,
    CredId2 = <<"some credential id2">>,
    CredId3 = <<"some credential id3">>,
    CredId6 = <<"some credential id6">>,

    {ok, Pid} = watts_plugin:start_link(),
    {ok, #{}} = watts_plugin:revoke(CredId1, UserInfo1),
    {error, #{user_msg := _, log_msg := _}} =
        watts_plugin:revoke(CredId2, UserInfo1),
    {error, #{user_msg := _, log_msg := _}} =
        watts_plugin:revoke(CredId3, UserInfo1),
    ?UNSETCONFIG( allow_dropping_credentials),
    {error, #{user_msg := _, log_msg := _}} =
        watts_plugin:revoke(CredId6, UserInfo1),
    ?SETCONFIG( allow_dropping_credentials, true),
    {ok, #{}} =
        watts_plugin:revoke(CredId6, UserInfo1),
    ?UNSETCONFIG( allow_dropping_credentials),
    ok = watts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = stop_meck(Meck),
    ok.

start_meck() ->
    Service1 = <<"ssh1">>,
    Service2 = <<"ssh2">>,
    Service3 = <<"ssh3">>,
    Service4 = <<"ssh4">>,
    Service5 = <<"ssh5">>,
    Service6 = <<"ssh6">>,
    CredId1 = <<"some credential id1">>,
    CredId2 = <<"some credential id2">>,
    CredId3 = <<"some credential id3">>,
    CredId4 = <<"some credential id4">>,
    CredId5 = <<"some credential id5">>,
    CredId6 = <<"some credential id6">>,
    Cred1State = <<"state1">>,
    Cred2State = <<"state1">>,
    Cred3State = <<"state1">>,
    Cred4State = <<"state1">>,
    Cred5State = <<"state1">>,
    Cred1 = #{name => "password", type => <<"text">>, value => <<"secret">>},
    Cred2 = #{name => "secret1", type => <<"textarea">>, value => <<"secret">>},
    Cred3 = #{name => "secret2", type => <<"textarea">>, value => #{id => <<"secret">>}},
    Cred4 = #{name => "secret3", type => <<"textfile">>, value => <<"secret">>},
    Cred5 = #{name => "secret4", type => <<"textfile">>, value => <<"secretlines">>},
    MyPid = self(),
    Interface = <<"test interface">>,

    CredState = <<"some_cred">>,
    UserId1 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoic3ViIn0">>,
    UserId2 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoic3UifQ">>,
    %% UserId3 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoi3ViIn0">>,
    MeckModules = [tts_data_sqlite, tts_plugin_sup, tts_plugin_runner, tts_service],
    ok = test_util:meck_new(MeckModules),
    GetFun = fun(_User) ->
                     {ok, [#{service_id => Service1}]}
             end,
    GetCredFun = fun(CredId) ->
                         case CredId of
                             CredId1 ->
                                 {ok, #{user_id => UserId1, service_id => Service1,
                                       cred_id => CredId, cred_state => Cred1State}};
                             CredId2 ->
                                 {ok, #{user_id => UserId1, service_id => Service2,
                                       cred_id => CredId, cred_state => Cred2State}};
                             CredId3 ->
                                 {ok, #{user_id => UserId1, service_id => Service3,
                                       cred_id => CredId, cred_state => Cred3State}};
                             CredId4 ->
                                 {ok, #{user_id => UserId1, service_id => Service4,
                                       cred_id => CredId, cred_state => Cred4State}};
                             CredId5 ->
                                 {ok, #{user_id => UserId1, service_id => Service5,
                                       cred_id => CredId, cred_state => Cred5State}};
                             CredId6 ->
                                 {ok, #{user_id => UserId1, service_id => Service6,
                                       cred_id => CredId, cred_state => Cred5State}};
                             _ ->
                                 {ok, #{user_id => no_one}}
                         end
                 end,
    GetCountFun = fun(User, Service) ->
                          io:format("userid: ~p",[User]),
                         case {User, Service} of
                             {UserId1, Service1} ->
                                 {ok, 1};
                             {UserId2, Service2} ->
                                 {ok, 101};
                             _ ->
                                 {ok, 0}
                         end
                 end,
    AddFun = fun(User, _Service, IFace, State, _AllowSameState) ->
                     IFace = Interface,
                     User = UserId1,
                     CredState = State,
                     CredId = <<"123">>,
                     {ok, CredId}
             end,
    DelFun = fun(UserId, _CredentialId) ->
                     case UserId of
                         UserId1 -> ok;
                         UserId2 -> ok;
                         _ -> {error, should_not_happen}
                    end
             end,
    RequestFun = fun(Service, UserInfo, _Par, _Queue, Pid) ->
                         {ok, User}  = watts_userinfo:return(id, UserInfo),
                         Pid = MyPid,
                         case {User, Service} of
                             {UserId1, Service1} ->
                                 {ok, #{result => <<"ok">>,
                                        credential => [Cred1],
                                        state => CredState}, []};
                             {UserId1, Service2} ->
                                 {ok, #{result => <<"ok">>,
                                        credential => [Cred2],
                                        state => CredState}, []};
                             {UserId1, Service3} ->
                                 {ok, #{result => <<"ok">>,
                                        credential => [Cred3],
                                        state => CredState}, []};
                             {UserId1, Service4} ->
                                 {ok, #{result => <<"ok">>,
                                        credential => [Cred4],
                                        state => CredState}, []};
                             {UserId1, Service5} ->
                                 {ok, #{result => <<"ok">>,
                                        credential => [Cred5],
                                        state => CredState}, []};
                             {UserId2, Service1} -> {ok, #{
                                               result => <<"error">>,
                                               user_msg => <<"some message">>,
                                               log_msg => <<"logged">>
                                               }, []};
                             {UserId2, Service3} -> {ok, #{
                                               user_msg => <<"some message">>,
                                               log_msg => <<"logged">>
                                               }, []};
                             {UserId2, Service4} -> {error, #{
                                               result => <<"error">>,
                                               log_msg => <<"logged">>
                                               }, []};
                             {UserId2, Service5} -> {ok, #{
                                               result => <<"ok">>,
                                               log_msg => <<"logged">>
                                               }, []};
                             %% {UserId3, _} -> {ok, #{credential => [Cred1]}, []};
                             _ -> {error, just_because, []}
                         end
                 end,
    RevokeFun = fun(Service, UserInfo, _Par, _Queue, Pid) ->
                         %% Service = ServiceId,
                        {ok, UserId} = watts_userinfo:return(id, UserInfo),
                        Pid = MyPid,
                        case {UserId, Service} of
                            {UserId1, Service1} -> {ok, #{result => <<"ok">>}, []};
                            {UserId1, Service2} -> {ok, #{result => <<"error">>,
                                                          user_msg => <<"user">>,
                                                          log_msg => <<"log">>
                                                         }, []};
                            {UserId1, Service3} -> {ok, #{result => <<"error">>,
                                                          log_msg => <<"log">>
                                                         }, []};
                            _ -> {error, just_because, []}
                        end
                end,
    AllowSame = fun(SerId) ->
                            case SerId of
                                Service1 -> true;
                                _ -> false
                            end
                    end,
    NewRunner = fun() -> {ok, self()} end,
    GetQueue = fun(_) -> {ok, queue_id} end,
    IsEnabled = fun(ServiceId) ->
                       not (Service6 == ServiceId)
                end,
    IsAllowd = fun(UserInfo, _) ->
                       {ok, UserId} = watts_userinfo:return(id, UserInfo),
                       case UserId of
                           UserId1 -> true;
                           UserId2 -> true;
                           _ -> false
                       end
               end,
    CredLimit = fun(_) -> {ok, 100} end,

    Exists = fun(ServiceId) ->
                    not( (ServiceId == Service6))
             end,
    ok = meck:expect(tts_data_sqlite, credential_get_list, GetFun),
    ok = meck:expect(tts_data_sqlite, credential_get, GetCredFun),
    ok = meck:expect(tts_data_sqlite, credential_get_count, GetCountFun),
    ok = meck:expect(tts_data_sqlite, credential_add, AddFun),
    ok = meck:expect(tts_data_sqlite, credential_remove, DelFun),
    ok = meck:expect(tts_plugin_sup, new_worker, NewRunner),
    ok = meck:expect(tts_plugin_runner, request, RequestFun),
    ok = meck:expect(tts_plugin_runner, revoke, RevokeFun),
    ok = meck:expect(tts_service, get_queue, GetQueue),
    ok = meck:expect(tts_service, is_enabled, IsEnabled),
    ok = meck:expect(tts_service, is_allowed, IsAllowd),
    ok = meck:expect(tts_service, get_credential_limit, CredLimit),
    ok = meck:expect(tts_service, allows_same_state, AllowSame),
    ok = meck:expect(tts_service, exists, Exists),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
