-module(tts_credential_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_credential:start_link(),
    ok = tts_credential:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

garbage_test() ->
    {ok, Pid} = tts_credential:start_link(),
    ignored = gen_server:call(Pid,unsupported_call),
    ok = gen_server:cast(Pid,unsupported_cast),
    Pid ! unsupported_msg,
    ok = tts_credential:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

%% get_list_test() ->
%%     ServiceId = <<"ssh1">>,
%%     OtherId = <<"ssh2">>,
%%     UserId = <<"joe">>,
%%     CredentialId = <<"cred1">>,
%%     OtherCred = <<"cred2">>,
%%     MeckModules = [tts_data_sqlite],
%%     GetFun = fun(User) ->
%%                      User = UserId,
%%                      {ok, [#{service_id => ServiceId}]}
%%              end,
%%     GetCredFun = fun(CredId) ->
%%                          case CredId of
%%                              <<"cred1">> ->
%%                                  {ok, #{user_id => <<"joe">>}};
%%                              _ ->
%%                                  {ok, #{user_id => no_one}}
%%                          end
%%                  end,
%%     GetCountFun = fun(User, Service) ->
%%                          case {User, Service} of
%%                              {<<"joe">>, <<"ssh1">>} ->
%%                                  {ok, 1};
%%                              _ ->
%%                                  {ok, 0}
%%                          end
%%                  end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_data_sqlite, credential_get_list, GetFun),
%%     ok = meck:expect(tts_data_sqlite, credential_get, GetCredFun),
%%     ok = meck:expect(tts_data_sqlite, credential_get_count, GetCountFun),


%%     {ok, Pid} = tts_credential:start_link(),
%%     ?assertEqual({ok, [#{service_id => ServiceId}]},
%%                  tts_credential:get_list(UserId)),
%%     ?assertEqual({ok, 1}, tts_credential:get_count(UserId, ServiceId)),
%%     ?assertEqual({ok, 0}, tts_credential:get_count(UserId, OtherId)),
%%     ?assertEqual(true, tts_credential:exists(UserId, CredentialId)),
%%     ?assertEqual(false, tts_credential:exists(UserId, OtherCred)),
%%     ok = tts_credential:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.

%% request_test() ->
%%     Service1 = <<"ssh1">>,
%%     Service2 = <<"ssh2">>,
%%     Service3 = <<"ssh3">>,
%%     Service4 = <<"ssh4">>,
%%     Service5 = <<"ssh5">>,
%%     UserId1 = <<"foo">>,
%%     UserId2 = <<"bar">>,
%%     UserId3 = <<"joe">>,
%%     UserId4 = <<"john">>,
%%     UserInfo1 = #{site => #{uid => UserId1}},
%%     UserInfo2 = #{site => #{uid => UserId2}},
%%     UserInfo3 = #{site => #{uid => UserId3}},
%%     UserInfo4 = #{site => #{uid => UserId4}},

%%     CredState = <<"some_cred">>,
%%     Cred1 = #{name => "password", type => <<"text">>, value => <<"secret">>},
%%     Cred2 = #{name => "secret file", type => <<"textarea">>, value => <<"secret">>},
%%     Cred3 = #{name => "secret file", type => <<"textarea">>, value => #{id => <<"secret">>}},
%%     Cred4 = #{name => "secret file", type => <<"textfile">>, value => <<"secret">>},
%%     Cred5 = #{name => "secret file", type => <<"textfile">>, value => <<"secretlines">>},
%%     MeckModules = [tts_data_sqlite, tts_cred_sup, tts_cred_worker, tts_service],
%%     Token = #{},
%%     Params = [],
%%     MyPid = self(),
%%     Interface = <<"test interface">>,
%%     AddFun = fun(User, _Service, IFace, State, _AllowMultiple) ->
%%                      IFace = Interface,
%%                      User = UserId1,
%%                      State = CredState,
%%                      CredId = <<"123">>,
%%                      {ok, CredId}
%%              end,
%%     RequestFun = fun(Service, UserInfo, _Par, Pid) ->
%%                          #{site := #{uid := User}} = UserInfo,
%%                          Pid = MyPid,
%%                          case {User, Service} of
%%                              {UserId1, Service1} ->
%%                                  {ok, #{credential => [Cred1],
%%                                         state => CredState}, []};
%%                              {UserId1, Service2} ->
%%                                  {ok, #{credential => [Cred2],
%%                                         state => CredState}, []};
%%                              {UserId1, Service3} ->
%%                                  {ok, #{credential => [Cred3],
%%                                         state => CredState}, []};
%%                              {UserId1, Service4} ->
%%                                  {ok, #{credential => [Cred4],
%%                                         state => CredState}, []};
%%                              {UserId1, Service5} ->
%%                                  {ok, #{credential => [Cred5],
%%                                         state => CredState}, []};
%%                              {UserId2,_} -> {ok, #{
%%                                                credential => [Cred1],
%%                                                error => <<>>}, []};
%%                              {UserId3, _} -> {ok, #{credential => [Cred1]}, []};
%%                              _ -> {error, just_because, []}
%%                          end
%%                  end,
%%     AllowSame = fun(SerId) ->
%%                             case SerId of
%%                                 Service1 -> true;
%%                                 _ -> false
%%                             end
%%                     end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_data_sqlite, credential_add, AddFun),
%%     ok = meck:expect(tts_data_sqlite, credential_get_count, fun(_, _)
%%                                                                -> {ok, 0} end),
%%     ok = meck:expect(tts_cred_sup, new_worker, fun() -> {ok, MyPid} end),
%%     ok = meck:expect(tts_cred_worker, request, RequestFun),
%%     ok = meck:expect(tts_service, is_enabled, fun(_) -> true end),
%%     ok = meck:expect(tts_service, get_credential_limit, fun(_)
%%                                                            -> {ok, 100} end),
%%     ok = meck:expect(tts_service, allows_same_state, AllowSame),


%%     {ok, Pid} = tts_credential:start_link(),
%%     {ok, [_, Cred1], []} = tts_credential:request(Service1, UserInfo1,
%%                                                  Interface, Token, Params),

%%     {ok, [_, Cred2], []} = tts_credential:request(Service2, UserInfo1,
%%                                                  Interface, Token, Params),

%%     {ok, [_, _Cred3], []} = tts_credential:request(Service3, UserInfo1,
%%                                                  Interface, Token, Params),

%%     {ok, [_, Cred4], []} = tts_credential:request(Service4, UserInfo1,
%%                                                  Interface, Token, Params),

%%     {ok, [_, Cred5], []} = tts_credential:request(Service5, UserInfo1,
%%                                                  Interface, Token, Params),

%%     {error, {script, <<>>}, []} = tts_credential:request(Service1, UserInfo2,
%%                                                          Interface, Token,
%%                                                          Params),
%%     {error, {missing_state, Service1},  []} = tts_credential:request(Service1,
%%                                                                       UserInfo3,
%%                                                                       Interface,
%%                                                                       Token,
%%                                                                       Params),
%%     {error, {internal, just_because}, []} = tts_credential:request(Service1,
%%                                                                    UserInfo4,
%%                                                                    Interface,
%%                                                                    Token,
%%                                                                    Params),


%%     ok = tts_credential:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.

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
%%     MeckModules = [tts_data_sqlite, tts_cred_sup, tts_cred_worker],
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
%%     ok = meck:expect(tts_cred_sup, new_worker, fun() -> {ok, MyPid} end),
%%     ok = meck:expect(tts_cred_worker, revoke, RevokeFun),

%%     {ok, Pid} = tts_credential:start_link(),
%%     {ok, RevokeResult, []} = tts_credential:revoke(CredId1, UserInfo1),
%%     {error, {script, <<>>}, []} = tts_credential:revoke(CredId2, UserInfo2),
%%     {error, {internal, just_because}, []} = tts_credential:revoke(CredId3, UserInfo3),
%%     {error, not_found, []} =  tts_credential:revoke(CredId4, UserInfo4),
%%     {error, bad_user, []} = tts_credential:revoke(CredId5, UserInfo4),
%%     ok = tts_credential:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.
