-module(watts_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

-define(ISSUER_URL, <<"https:://issuer.sample">>).
-define(ISSUER2_URL, <<"https:://issuer.other">>).

error_session_test() ->
    {ok, Meck} = start_meck(),
    try
        EMsg = <<"this is some error message">>,
        {ok, Pid} = watts:session_with_error(EMsg),
        watts_session:close(Pid),
        test_util:wait_for_process_to_die(Pid, 100)
    after
        stop_meck(Meck)
    end,
    ok.


login_and_out_test() ->
    {ok, Meck} = start_meck(),
    try
        Issuer = ?ISSUER_URL,
        GoodAccessToken = <<"accesstoken">>,
        BadAccessToken = "token",
        BadOidcToken = #{},
        GoodOidcToken = #{id => #{claims => #{sub => <<"sub">>, iss => Issuer}},
                          access => #{token => GoodAccessToken},
                          cookies => []
                         },
        {error, bad_token} = watts:login_with_oidcc(BadOidcToken),
        {error, bad_token} = watts:login_with_access_token(BadAccessToken, Issuer),
        {ok, #{session_pid := Pid1}} = watts:login_with_oidcc(GoodOidcToken),
        {ok, #{session_pid := Pid2}} = watts:login_with_access_token(GoodAccessToken,
                                                                     Issuer),

        ok = watts:logout(Pid1),
        ok = watts:logout(Pid2),
        test_util:wait_for_process_to_die(Pid1, 100),
        test_util:wait_for_process_to_die(Pid2, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

additional_login_test() ->
    {ok, Meck} = start_meck(),
    try
        Issuer = ?ISSUER_URL,
        Issuer2 = ?ISSUER2_URL,
        OidcToken1 = #{id => #{claims => #{sub => <<"sub">>, iss => Issuer}},
                       access => #{token => <<"accestoken">>},
                       cookies => []
                      },
        {ok, #{session_pid := Pid}} = watts:login_with_oidcc(OidcToken1),

        {ok, Token} = watts_session:get_sess_token(Pid),
        OidcToken2 = #{id => #{claims => #{sub => <<"sub">>, iss => Issuer2}},
                       access => #{token => <<"accestoken">>},
                       cookies => [{watts_http_util:cookie_name(), Token}]
                      },
        Params = [],
        ServiceId = <<"service0815">>,
        Provider = <<"ID3">>,
        ok = watts_session:set_redirection(ServiceId, Params, Provider, Pid),
        {ok, #{session_pid := Pid}} = watts:login_with_oidcc(OidcToken2),
        {ok, UserInfo} = watts_session:get_user_info(Pid),
        true = watts_userinfo:has_additional_login(ServiceId, Provider, UserInfo),

        ok = watts:logout(Pid),
        test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.


proxy_function_test() ->
    {ok, {Session, _} = Meck} = start_meck(),

    try
        ?assertEqual(true, watts:does_credential_exist(<<"id">>, Session)),
        ?assertEqual(true, watts:does_temp_cred_exist(<<"id">>, Session)),
        {ok, [#{id := <<"id">>}|_]} = watts:get_openid_provider_list(),
        ?assertEqual({ok, []}, watts:get_service_list_for(Session)),
        ?assertEqual({ok, []}, watts:get_credential_list_for(Session)),
        ?assertEqual({ok, <<"accesstoken">>}, watts:get_access_token_for(Session)),
        ?assertEqual({ok, <<"Nice Name">>}, watts:get_display_name_for(Session)),
        ?assertEqual({ok, <<"TempCredId">>}, watts:store_temp_cred(<<"credential">>,
                                                                   Session)),
        ?assertEqual({ok, <<"credential">>}, watts:get_temp_cred(<<"TempCredId">>,
                                                                 Session))
    after
        ok = stop_meck(Meck)
    end,
    ok.



request_test() ->
    {ok, {Session, _} = Meck} = start_meck(),
    try
        {ok, _} = watts:request_credential_for(<<"service">>, Session, []),
        {error, _} = watts:request_credential_for(<<"other_service">>, Session, []),
        {ok, #{result := oidc_login}} = watts:request_credential_for(
                                          <<"yet_another_service">>, Session, []),
        {error, _} = watts:request_credential_for(<<"error">>, Session, []),
        {error, _} = watts:request_credential_for(<<"bad">>, Session, [])
    after
        ok = stop_meck(Meck)
    end,
    ok.

revoke_test() ->
    {ok, {Session, _} = Meck} = start_meck(),
    try
        ok = watts:revoke_credential_for(<<"cred1">>, Session),
        {error, _} = watts:revoke_credential_for(<<"cred2">>, Session)
    after
        ok = stop_meck(Meck)
    end,
    ok.

get_provider_info_test() ->
    {ok, Meck} = start_meck(),
    try
        Id = <<"ID1">>,
        ExpectedInfo = #{
          id => Id,
          description => <<"info">>,
          ready => false,
          extra_config => #{priority => 5},
          issuer => ?ISSUER_URL
         },
        ?assertEqual({ok, ExpectedInfo}, watts:get_openid_provider_info(Id))
    after
        ok = stop_meck(Meck)
    end.



start_meck() ->
    ?SETCONFIG(provider_list, [#{id => <<"id">>,
                                 disable_login => false},
                               #{id => <<"ID1">>,
                                 disable_login => false},
                               #{id => <<"ID2">>,
                                 disable_login => false},
                               #{id => <<"ID3">>,
                                 disable_login => false}
                              ]),
    MeckModules = [watts_session_mgr, oidcc, watts_plugin, watts_temp_cred,
                   watts_service],
    CredId = <<"cred1">>,
    Credential = #{id => CredId,
                   entries => [
                  #{name => <<"user">>, type => text, value => <<"joe">>},
                  #{name => <<"password">>, type => text, value => <<"secret">>}]
                 },

    ErrMsg = #{user_msg => <<"msg1">>, log_msg => <<"msg2">>},

    TestToken = <<"some session token">>,
    TestSession = watts_session:start_link(TestToken),

    ProviderList = fun() ->
                             {ok, [ {<<"id">>, pid1},
                                    {<<"ID1">>, pid2},
                                    {<<"ID2">>, pid3},
                                    {<<"ID3">>, pid4}]}
                     end,

    RetrieveUserInfo = fun(_, _, _) ->
                               {ok, #{sub => <<"sub">>}}
                       end,
    ProviderInfo = fun(<<"ID2">>) ->
                           {ok, #{
                                id => <<"ID2">>,
                                description => <<"a ready provider">>,
                                ready => true,
                                extra_config => #{priority => 5},
                                issuer => ?ISSUER_URL}};
                      (<<"ID3">>) ->
                           {ok, #{
                                id => <<"ID3">>,
                                description => <<"a second provider">>,
                                ready => true,
                                extra_config => #{priority => 5},
                                issuer => ?ISSUER2_URL}};
                      (_) ->
                             {ok, #{
                                id => <<"ID1">>,
                                description => <<"info">>,
                                ready => false,
                                extra_config => #{priority => 5},
                                issuer => ?ISSUER_URL}}
                     end,
    FindProvider = fun(Issuer) ->
                           case Issuer of
                               ?ISSUER_URL -> {ok, provider_pid};
                               ?ISSUER2_URL -> {ok, <<"ID3">>};
                               _ -> {error, not_found}
                           end
                   end,
    NewSession = fun() ->
                         TestSession
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
                                        {error, ErrMsg};
                                    <<"other_service">> ->
                                        {oidc_login, #{provider => <<"ID1">>,
                                                       msg => <<"Requires login">>}};
                                    <<"yet_another_service">> ->
                                        {oidc_login, #{provider => <<"ID2">>,
                                                       msg => <<"Requires login">>}};
                                    _ ->
                                        {error, internal}
                                end
                           end,
    CredentialRevoke = fun(CId, _UserInfo) ->
                                case CId of
                                    CredId ->
                                        {ok, #{}};
                                    _ ->
                                        {error, ErrMsg}
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
    GetSession = fun(Id) ->
                         case Id of
                             undefined -> {ok, Id};
                             <<"some session token">> -> TestSession
                         end
                 end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts_session_mgr, new_session, NewSession),
    ok = meck:expect(watts_session_mgr, session_terminating, fun(_) -> ok end),
    ok = meck:expect(watts_session_mgr, get_session, GetSession),
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
    ok = watts_session:set_type(oidc, SessionPid),
    {ok, {SessionPid, MeckModules}}.


stop_meck({SessionPid, MeckModules}) ->
    ?UNSETCONFIG(provider_list),
    watts_session:close(SessionPid),
    test_util:wait_for_process_to_die(SessionPid, 100),
    ok = test_util:meck_done(MeckModules),
    ok.
