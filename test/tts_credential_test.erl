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

get_list_test() ->
    ServiceId = <<"ssh1">>,
    UserId = <<"joe">>,
    MeckModules = [tts_data_sqlite],
    GetFun = fun(User) ->
                     User = UserId,
                     {ok, [{ServiceId, #{}}]}
             end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_data_sqlite, credential_get, GetFun),


    {ok, Pid} = tts_credential:start_link(),
    {ok, [ServiceId]} = tts_credential:get_list(UserId),
    ok = tts_credential:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

request_test() ->
    ServiceId = <<"ssh1">>,
    UserId1 = <<"foo">>,
    UserId2 = <<"bar">>,
    UserId3 = <<"joe">>,
    UserInfo1 = #{uid => UserId1},
    UserInfo2 = #{uid => UserId2},
    UserInfo3 = #{uid => UserId3},
    CredState = <<"some_cred">>,
    Cred = <<"secret">>,
    MeckModules = [tts_data_sqlite, tts_cred_sup, tts_cred_worker],
    Token = #{},
    Params = [],
    MyPid = self(),
    AddFun = fun(User, Service, State) ->
                     User = UserId1,
                     Service = ServiceId,
                     State = CredState,
                     ok
             end,
    RequestFun = fun(Service, UserInfo, _Par, Pid) ->
                         Service = ServiceId,
                         #{uid := User} = UserInfo,
                         Pid = MyPid,
                         case User of
                             UserId1 -> {ok, #{credential => Cred,
                                              state => CredState}, []};
                             UserId2 -> {ok, #{
                                           credential => Cred,
                                           error => <<>>}, []};
                             _ -> {error, just_because, []}
                         end
                 end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_data_sqlite, credential_add, AddFun),
    ok = meck:expect(tts_cred_sup, new_worker, fun() -> {ok, MyPid} end),
    ok = meck:expect(tts_cred_worker, request, RequestFun),


    {ok, Pid} = tts_credential:start_link(),
    {ok, Cred, []} = tts_credential:request(ServiceId, UserInfo1, Token, Params),
    {error, {script, <<>>}, []} = tts_credential:request(ServiceId, UserInfo2, Token, Params),
    {error, {internal, just_because}, []} = tts_credential:request(ServiceId, UserInfo3, Token, Params),
    ok = tts_credential:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

revoke_test() ->
    ServiceId = <<"ssh1">>,
    UserId1 = <<"foo">>,
    UserId2 = <<"bar">>,
    UserId3 = <<"joe">>,
    UserId4 = <<"ann">>,
    UserInfo1 = #{uid => UserId1},
    UserInfo2 = #{uid => UserId2},
    UserInfo3 = #{uid => UserId3},
    UserInfo4 = #{uid => UserId4},
    CredState = <<"some_cred">>,
    Cred = <<"secret">>,
    MeckModules = [tts_data_sqlite, tts_cred_sup, tts_cred_worker],
    MyPid = self(),
    GetFun = fun(UserId) ->
                     case UserId of
                         UserId1 -> {ok, [{ServiceId, CredState}]};
                         UserId2 -> {ok, [{ServiceId, CredState}]};
                         UserId3 -> {ok, [{ServiceId, CredState}]};
                         _ -> {ok, []}
                     end
             end,
    DelFun = fun(UserId, Service, State) ->
                     Service = ServiceId,
                     State = CredState,
                     case UserId of
                         UserId1 -> ok;
                         UserId2 -> ok;
                         UserId3 -> ok;
                         _ -> {error, should_not_happen}
                    end
             end,
    RevokeFun = fun(Service, UserInfo, _Par, Pid) ->
                         Service = ServiceId,
                         #{uid := User} = UserInfo,
                         Pid = MyPid,
                         case User of
                             UserId1 -> {ok, #{result => Cred,
                                              state => CredState}, []};
                             UserId2 -> {ok, #{
                                           result => Cred,
                                           error => <<>>}, []};
                             _ -> {error, just_because, []}
                         end
                 end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_data_sqlite, credential_remove, DelFun),
    ok = meck:expect(tts_data_sqlite, credential_get, GetFun),
    ok = meck:expect(tts_cred_sup, new_worker, fun() -> {ok, MyPid} end),
    ok = meck:expect(tts_cred_worker, revoke, RevokeFun),

    {ok, Pid} = tts_credential:start_link(),
    {ok, Cred, []} = tts_credential:revoke(ServiceId, UserInfo1),
    {error, {script, <<>>}, []} = tts_credential:revoke(ServiceId, UserInfo2),
    {error, {internal, just_because}, []} = tts_credential:revoke(ServiceId, UserInfo3),
    Result =  tts_credential:revoke(ServiceId, UserInfo4),
    io:format("Result: ~p~n",[Result]),
    Result = {error, not_found},
    ok = tts_credential:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.
