-module(watts_test).
-include_lib("eunit/include/eunit.hrl").


-define(ISSUER_URL, <<"https:://issuer.sample">>).

error_session_test() ->
    {ok, Meck} = start_meck(),
    EMsg = <<"this is some error message">>,
    {ok, Pid} = watts:session_with_error(EMsg),
    watts_session:close(Pid),
    test_util:wait_for_process_to_die(Pid, 100),
    stop_meck(Meck),
    ok.


login_and_out_test() ->
    {ok, Meck} = start_meck(),
    BadOidcToken = #{},
    GoodOidcToken = #{id => #{claims => #{sub => <<"sub">>, iss => <<"iss">>}}},
    BadAccessToken = "token",
    GoodAccessToken = <<"accesstoken">>,
    Issuer = ?ISSUER_URL,
    {error, bad_token} = watts:login_with_oidcc(BadOidcToken),
    {error, bad_token} = watts:login_with_access_token(BadAccessToken, Issuer),
    {ok, #{session_pid := Pid1}} = watts:login_with_oidcc(GoodOidcToken),
    {ok, #{session_pid := Pid2}} = watts:login_with_access_token(GoodAccessToken,
                                                                 Issuer),

    ok = watts:logout(Pid1),
    ok = watts:logout(Pid2),
    test_util:wait_for_process_to_die(Pid1, 100),
    test_util:wait_for_process_to_die(Pid2, 100),
    stop_meck(Meck),
    ok.

proxy_function_test() ->
    {ok, {Session, _} = Meck} = start_meck(),

    ?assertEqual(true, watts:does_credential_exist(<<"id">>, Session)),
    ?assertEqual(true, watts:does_temp_cred_exist(<<"id">>, Session)),
    {ok, [#{id := <<"id">>}]} = watts:get_openid_provider_list(),
    ?assertEqual({ok, []}, watts:get_service_list_for(Session)),
    ?assertEqual({ok, []}, watts:get_credential_list_for(Session)),
    ?assertEqual({ok, <<"accesstoken">>}, watts:get_access_token_for(Session)),
    ?assertEqual({ok, <<"Nice Name">>}, watts:get_display_name_for(Session)),
    ?assertEqual({ok, <<"TempCredId">>}, watts:store_temp_cred(<<"credential">>,
                                                             Session)),
    ?assertEqual({ok, <<"credential">>}, watts:get_temp_cred(<<"TempCredId">>,
                                                             Session)),

    stop_meck(Meck),
    ok.



request_test() ->
    {ok, {Session, _} = Meck} = start_meck(),
    {ok, _} = watts:request_credential_for(<<"service">>, Session, [], <<"test interface">>),
    {error, _} = watts:request_credential_for(<<"error">>, Session, [], <<"test interface">>),
    {error, _} = watts:request_credential_for(<<"bad">>, Session, [], <<"test interface">>),
    stop_meck(Meck),
    ok.

revoke_test() ->
    {ok, {Session, _} = Meck} = start_meck(),
    ok = watts:revoke_credential_for(<<"cred1">>, Session),
    {error, _} = watts:revoke_credential_for(<<"cred2">>, Session),
    stop_meck(Meck),
    ok.





start_meck() ->
    MeckModules = [watts_session_mgr, oidcc, watts_plugin, watts_temp_cred,
                   watts_service],
    CredId = <<"cred1">>,
    Credential = #{id => CredId,
                   entries => [
                  #{name => <<"user">>, type => text, value => <<"joe">>},
                  #{name => <<"password">>, type => text, value => <<"secret">>}]
                 },

    ProviderList = fun() ->
                             {ok, [ {<<"id">>, pid1} ]}
                     end,

    RetrieveUserInfo = fun(_, _) ->
                               {ok, #{sub => <<"sub">>}}
                       end,
    ProviderInfo = fun(_Pid) ->
                             {ok, #{
                                id => <<"ID1">>,
                                description => <<"info">>,
                                ready => false,
                                issuer => ?ISSUER_URL}}
                     end,
    FindProvider = fun(_Issuer) ->
                           {ok, provider_pid}
                     end,
    NewSession = fun() ->
                         watts_session:start_link(<<"some session token">>)
                 end,
    Exists = fun(_, _) ->
                     true
             end,
    CredGetList = fun(_UserInfo) ->
                             {ok, []}
                     end,
    CredentialRequest = fun(SerId, _UserInfo, _IFace, _Param) ->
                                case SerId of
                                    <<"service">> ->
                                        {ok, Credential};
                                    <<"error">> ->
                                        {error, Credential};
                                    _ ->
                                        {error, internal}
                                end
                           end,
    CredentialRevoke = fun(CId, _UserInfo) ->
                                case CId of
                                    CredId ->
                                        {ok, #{}};
                                    _ ->
                                        {error, #{log_msg => <<"logging">>}}
                                end
                           end,
    TempCredAdd = fun(_Cred, _UserId) ->
                          {ok, <<"TempCredId">>}
                  end,
    TempCredGet = fun(_Cred, _UserId) ->
                          {ok, <<"credential">>}
                  end,
    ServiceGetList = fun(_Uid) ->
                             {ok, []}
                     end,
    ServiceEnabled = fun(_Uid) ->
                             true
                     end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts_session_mgr, new_session, NewSession),
    ok = meck:expect(watts_session_mgr, session_terminating, fun(_) -> ok end),
    ok = meck:expect(oidcc, get_openid_provider_list, ProviderList),
    ok = meck:expect(oidcc, retrieve_user_info, RetrieveUserInfo),
    ok = meck:expect(oidcc, get_openid_provider_info, ProviderInfo),
    ok = meck:expect(oidcc, find_openid_provider, FindProvider),
    ok = meck:expect(watts_plugin, exists, Exists),
    ok = meck:expect(watts_plugin, get_cred_list, CredGetList),
    ok = meck:expect(watts_plugin, request, CredentialRequest),
    ok = meck:expect(watts_plugin, revoke, CredentialRevoke),
    ok = meck:expect(watts_temp_cred, exists, Exists),
    ok = meck:expect(watts_temp_cred, add_cred, TempCredAdd),
    ok = meck:expect(watts_temp_cred, get_cred, TempCredGet),
    ok = meck:expect(watts_service, get_list, ServiceGetList),
    ok = meck:expect(watts_service, is_enabled, ServiceEnabled),



    {ok, SessionPid} = watts_session:start_link(<<"some id">>),
    Issuer = ?ISSUER_URL,
    Subject = <<"sub">>,
    Token = #{access => #{token => <<"accesstoken">>},
              user_info => #{ name => <<"Nice Name">> , sub => <<"sub">>}
             },
    ok = watts_session:set_iss_sub(Issuer, Subject, SessionPid),
    ok = watts_session:set_token(Token, SessionPid),
    {ok, {SessionPid, MeckModules}}.


stop_meck({SessionPid, MeckModules}) ->
    watts_session:close(SessionPid),
    test_util:wait_for_process_to_die(SessionPid, 100),
    ok = test_util:meck_done(MeckModules),
    ok.
