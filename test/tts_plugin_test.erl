-module(tts_plugin_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_plugin:start_link(),
    ok = tts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = tts_plugin:start_link(),
    ignored = gen_server:call(Pid,unsupported_call),
    ok = gen_server:cast(Pid,unsupported_cast),
    Pid ! unsupported_msg,
    ok = tts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

get_list_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"ssh1">>,
    OtherId = <<"ssh2">>,
    CredentialId = <<"cred1">>,
    OtherCred = <<"cred2">>,

    {ok, UserInfo0} = tts_userinfo:new(),
    {ok, UserInfo} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),
    {ok, Pid} = tts_plugin:start_link(),

    ?assertEqual({ok, [#{service_id => ServiceId}]},
                 tts_plugin:get_cred_list(UserInfo)),
    ?assertEqual({ok, 1}, tts_plugin:get_count(UserInfo, ServiceId)),
    ?assertEqual({ok, 0}, tts_plugin:get_count(UserInfo, OtherId)),
    ?assertEqual(true, tts_plugin:exists(UserInfo, CredentialId)),
    ?assertEqual(false, tts_plugin:exists(UserInfo, OtherCred)),
    ok = tts_plugin:stop(),
    test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.

request_test() ->
    Service1 = <<"ssh1">>,
    Service2 = <<"ssh2">>,
    Service3 = <<"ssh3">>,
    Service4 = <<"ssh4">>,
    Service5 = <<"ssh5">>,
    Cred1 = #{name => "password", type => <<"text">>, value => <<"secret">>},
    Cred2 = #{name => "secret file", type => <<"textarea">>, value => <<"secret">>},
    Cred3 = #{name => "secret file", type => <<"textarea">>, value => #{id => <<"secret">>}},
    Cred4 = #{name => "secret file", type => <<"textfile">>, value => <<"secret">>},
    Cred5 = #{name => "secret file", type => <<"textfile">>, value => <<"secretlines">>},
    Interface = <<"test interface">>,
    Params = [],

    {ok, Meck} = start_meck(),

    {ok, UserInfo0} = tts_userinfo:new(),
    {ok, UserInfo1} = tts_userinfo:update_iss_sub(<<"iss">>, <<"sub">>, UserInfo0),

    {ok, Pid} = tts_plugin:start_link(),
    {ok, #{id := <<"123">>, entries := [Cred1]}} =
        tts_plugin:request(Service1, UserInfo1, Interface, Params),

    {ok, #{id := <<"123">>, entries := [Cred2]}} =
        tts_plugin:request(Service2, UserInfo1, Interface, Params),

    %% {ok, #{id := <<"123">>, entries := [Cred3]}} =
    %%     tts_plugin:request(Service3, UserInfo1, Interface, Params),

    {ok, #{id := <<"123">>, entries := [Cred4]}} =
        tts_plugin:request(Service4, UserInfo1, Interface, Params),

    {ok, #{id := <<"123">>, entries := [Cred5]}} =
        tts_plugin:request(Service5, UserInfo1, Interface, Params),

    %% {error, {script, <<>>}, []} = tts_plugin:request(Service1, UserInfo2,
    %%                                                      Interface, Token,
    %%                                                      Params),
    %% {error, {missing_state, Service1},  []} = tts_plugin:request(Service1,
    %%                                                                   UserInfo3,
    %%                                                                   Interface,
    %%                                                                   Token,
    %%                                                                   Params),
    %% {error, {internal, just_because}, []} = tts_plugin:request(Service1,
    %%                                                                UserInfo4,
    %%                                                                Interface,
    %%                                                                Token,
    %%                                                                Params),


    ok = tts_plugin:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),

    ok = stop_meck(Meck),
    ok.

%% revoke_test() ->
%%     CredId1 = <<"some credential id1">>,
%%     CredId2 = <<"some credential id2">>,
%%     CredId3 = <<"some credential id3">>,
%%     CredId4 = <<"some credential id4">>,
%%     CredId5 = <<"some credential id5">>,
%%     ServiceId = <<"ssh">>,
%%     UserId1 = <<"foo">>,
%%     UserId2 = <<"bar">>,
%%     UserId3 = <<"joe">>,
%%     UserId4 = <<"ann">>,
%%     UserInfo1 = #{site => #{uid => UserId1}},
%%     UserInfo2 = #{site => #{uid => UserId2}},
%%     UserInfo3 = #{site => #{uid => UserId3}},
%%     UserInfo4 = #{site => #{uid => UserId4}},

%%     CredState = <<"some_cred">>,
%%     RevokeResult = <<"some info">>,
%%     MeckModules = [tts_data_sqlite, tts_plugin_sup, tts_cred_worker],
%%     MyPid = self(),
%%     Cred = #{ service_id => ServiceId, cred_state => CredState},
%%     GetFun = fun(CredentialId) ->
%%                      NewCred = maps:put(cred_id, CredentialId, Cred),
%%                      case CredentialId of
%%                          CredId1 -> {ok, maps:put(user_id, UserId1, NewCred)};
%%                          CredId2 -> {ok, maps:put(user_id, UserId2, NewCred)};
%%                          CredId3 -> {ok, maps:put(user_id, UserId3, NewCred)};
%%                          CredId4 -> {error, not_found};
%%                          _ -> {ok, maps:put(userid, UserId1, NewCred)}
%%                      end
%%              end,
%%     DelFun = fun(UserId, _CredentialId) ->
%%                      case UserId of
%%                          UserId1 -> ok;
%%                          UserId2 -> ok;
%%                          UserId3 -> ok;
%%                          _ -> {error, should_not_happen}
%%                     end
%%              end,
%%     RevokeFun = fun(Service, UserInfo, _Par, Pid) ->
%%                          Service = ServiceId,
%%                          #{site := #{uid := User}} = UserInfo,
%%                          Pid = MyPid,
%%                          case User of
%%                              UserId1 -> {ok, #{result => RevokeResult,
%%                                               state => CredState}, []};
%%                              UserId2 -> {ok, #{
%%                                            result => RevokeResult,
%%                                            error => <<>>}, []};
%%                              _ -> {error, just_because, []}
%%                          end
%%                  end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_data_sqlite, credential_remove, DelFun),
%%     ok = meck:expect(tts_data_sqlite, credential_get, GetFun),
%%     ok = meck:expect(tts_plugin_sup, new_worker, fun() -> {ok, MyPid} end),
%%     ok = meck:expect(tts_cred_worker, revoke, RevokeFun),

%%     {ok, Pid} = tts_plugin:start_link(),
%%     {ok, RevokeResult, []} = tts_plugin:revoke(CredId1, UserInfo1),
%%     {error, {script, <<>>}, []} = tts_plugin:revoke(CredId2, UserInfo2),
%%     {error, {internal, just_because}, []} = tts_plugin:revoke(CredId3, UserInfo3),
%%     {error, not_found, []} =  tts_plugin:revoke(CredId4, UserInfo4),
%%     {error, bad_user, []} = tts_plugin:revoke(CredId5, UserInfo4),
%%     ok = tts_plugin:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.

start_meck() ->
    Service1 = <<"ssh1">>,
    Service2 = <<"ssh2">>,
    Service3 = <<"ssh3">>,
    Service4 = <<"ssh4">>,
    Service5 = <<"ssh5">>,
    Cred1 = #{name => "password", type => <<"text">>, value => <<"secret">>},
    Cred2 = #{name => "secret file", type => <<"textarea">>, value => <<"secret">>},
    Cred3 = #{name => "secret file", type => <<"textarea">>, value => #{id => <<"secret">>}},
    Cred4 = #{name => "secret file", type => <<"textfile">>, value => <<"secret">>},
    Cred5 = #{name => "secret file", type => <<"textfile">>, value => <<"secretlines">>},
    MyPid = self(),
    Interface = <<"test interface">>,

    CredState = <<"some_cred">>,
    UserId1 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoic3ViIn0">>,
    %% UserId2 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoi3ViIn0">>,
    %% UserId3 =  <<"eyJpc3N1ZXIiOiJpc3MiLCJzdWJqZWN0Ijoi3ViIn0">>,
    MeckModules = [tts_data_sqlite, tts_plugin_sup, tts_plugin_runner, tts_service],
    ok = test_util:meck_new(MeckModules),
    GetFun = fun(_User) ->
                     {ok, [#{service_id => Service1}]}
             end,
    GetCredFun = fun(CredId) ->
                         case CredId of
                             <<"cred1">> ->
                                 {ok, #{user_id => UserId1}};
                             _ ->
                                 {ok, #{user_id => no_one}}
                         end
                 end,
    GetCountFun = fun(User, Service) ->
                          io:format("userid: ~p",[User]),
                         case {User, Service} of
                             {UserId1, <<"ssh1">>} ->
                                 {ok, 1};
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
    RequestFun = fun(Service, UserInfo, _Par, _Queue, Pid) ->
                         {ok, User}  = tts_userinfo:return(id, UserInfo),
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
                             %% {UserId2,_} -> {ok, #{
                             %%                   credential => [Cred1],
                             %%                   error => <<>>}, []};
                             %% {UserId3, _} -> {ok, #{credential => [Cred1]}, []};
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
    IsEnabled = fun(_) -> true end,
    IsAllowd = fun(_, _) -> true end,
    CredLimit = fun(_) -> {ok, 100} end,

    ok = meck:expect(tts_data_sqlite, credential_get_list, GetFun),
    ok = meck:expect(tts_data_sqlite, credential_get, GetCredFun),
    ok = meck:expect(tts_data_sqlite, credential_get_count, GetCountFun),
    ok = meck:expect(tts_data_sqlite, credential_add, AddFun),
    ok = meck:expect(tts_plugin_sup, new_worker, NewRunner),
    ok = meck:expect(tts_plugin_runner, request, RequestFun),
    ok = meck:expect(tts_service, get_queue, GetQueue),
    ok = meck:expect(tts_service, is_enabled, IsEnabled),
    ok = meck:expect(tts_service, is_allowed, IsAllowd),
    ok = meck:expect(tts_service, get_credential_limit, CredLimit),
    ok = meck:expect(tts_service, allows_same_state, AllowSame),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
