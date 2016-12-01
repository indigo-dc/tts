-module(tts_test).
-include_lib("eunit/include/eunit.hrl").



%% login_test() ->
%%     MeckModules = [tts_session_mgr, tts_session, oidcc],

%%     Issuer = <<"https://tts.provider">>,
%%     Subject = <<"230403">>,

%%     BadToken = #{},
%%     OidcToken = #{id => #{claims => #{sub => Subject,
%%                                       iss => Issuer
%%                                      }}},
%%     CrashToken = #{id => #{claims => #{sub => Subject,
%%                                       iss => undefined
%%                                      }}},
%%     AccessToken = <<"some access token">>,

%%     NewSession = fun() ->
%%                          {ok, sess_pid}
%%                  end,


%%     SessionGetId = fun(_SessionPid) ->
%%                            {ok, sess_id}
%%                    end,

%%     SessionSetToken = fun(_TokenMap, _SessionPid) ->
%%                               ok
%%                       end,

%%     SessionSetUserInfo = fun(_UserInfo, _SessionPid) ->
%%                                  ok
%%                          end,

%%     SessionSetIssSub = fun(Iss, _Subject, _SessionPid) ->
%%                                case Iss of
%%                                    undefined ->
%%                                        {error, bad_issuer};
%%                                    _ -> ok
%%                                end
%%                          end,

%%     SessionIsLoggedIn = fun(_SessionPid) ->
%%                                 true
%%                         end,

%%     SessionClose = fun(_SessionPid) ->
%%                            ok
%%                    end,
%%     FindProvider = fun(_Issuer) ->
%%                            {ok, provider_pid}
%%                    end,
%%     ProviderInfo = fun(_Pid) ->
%%                            {ok, #{issuer => Issuer}}
%%                    end,
%%     RetrieveUserInfo = fun(_AccessToken, _ProviderPid) ->
%%                                {ok, #{sub => Subject}}
%%                        end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_session_mgr, new_session, NewSession),
%%     ok = meck:expect(tts_session, get_id, SessionGetId),
%%     ok = meck:expect(tts_session, set_token, SessionSetToken),
%%     ok = meck:expect(tts_session, set_user_info, SessionSetUserInfo),
%%     ok = meck:expect(tts_session, set_iss_sub, SessionSetIssSub),
%%     ok = meck:expect(tts_session, is_logged_in, SessionIsLoggedIn),
%%     ok = meck:expect(tts_session, close, SessionClose),
%%     ok = meck:expect(oidcc, find_openid_provider, FindProvider),
%%     ok = meck:expect(oidcc, get_openid_provider_info, ProviderInfo),
%%     ok = meck:expect(oidcc, retrieve_user_info, RetrieveUserInfo),

%%     OidcResult = tts:login_with_oidcc(OidcToken),
%%     ?assertEqual({ok, #{session_id => sess_id, session_pid => sess_pid}},
%%                  OidcResult),
%%     BadOidcResult = tts:login_with_oidcc(BadToken),
%%     ?assertEqual({error, bad_token}, BadOidcResult),

%%     CrashOidcResult = tts:login_with_oidcc(CrashToken),
%%     ?assertEqual({error, internal}, CrashOidcResult),

%%     ACResult = tts:login_with_access_token(AccessToken, Issuer),
%%     ?assertEqual({ok, #{session_id => sess_id, session_pid => sess_pid}},
%%                  ACResult),
%%     BadACResult = tts:login_with_access_token(BadToken, Issuer),
%%     ?assertEqual({error, bad_token}, BadACResult),


%%     ok = test_util:meck_done(MeckModules),
%%     ok.


%% proxy_function_test() ->

%%     CredId = <<"cred1">>,
%%     OtherId = <<"other_id">>,
%%     Credential = [#{name => id, value=> CredId},
%%                   #{name => <<"user">>, type => text, value => <<"joe">>},
%%                   #{name => <<"password">>, type => text, value => <<"secret">>}
%%                  ],
%%     SessionPid = pid1,
%%     DisplayName = <<"User Name">>,

%%     setup_mecking(),

%%     ?assertEqual(true, tts:does_credential_exist(CredId, SessionPid)),
%%     ?assertEqual(false, tts:does_credential_exist(OtherId, SessionPid)),
%%     ?assertEqual({ok, [#{id => <<"id">>,
%%                          desc => <<"info">>,
%%                          issuer => <<"https://tts.issuer">>,
%%                          ready => false}]},
%%                  tts:get_openid_provider_list()),
%%     ?assertEqual(true, tts:does_temp_cred_exist(CredId, SessionPid)),
%%     ?assertEqual(false, tts:does_temp_cred_exist(OtherId, SessionPid)),
%%     ?assertEqual({ok, []}, tts:get_service_list_for(SessionPid)),
%%     ?assertEqual({ok, [CredId]}, tts:get_credential_list_for(SessionPid)),
%%     ?assertEqual({ok, <<"AccessToken">>}, tts:get_access_token_for(SessionPid)),
%%     ?assertEqual({ok, DisplayName}, tts:get_display_name_for(SessionPid)),
%%     ?assertEqual({ok, CredId}, tts:store_temp_cred(Credential, SessionPid)),
%%     ?assertEqual({ok, Credential}, tts:get_temp_cred(CredId, SessionPid)),

%%     stop_mecking(),
%%     ok.


%% request_credential_test() ->

%%     setup_mecking(),
%%     CredId = <<"cred1">>,
%%     Credential = [#{name => id, value=> CredId},
%%                   #{name => <<"user">>, type => text, value => <<"joe">>},
%%                   #{name => <<"password">>, type => text, value => <<"secret">>}
%%                  ],
%%     ?assertEqual({ok, Credential, []},
%%                   tts:request_credential_for(<<"good">>, session, [], <<"test">>)),
%%     {error, _, []} = tts:request_credential_for(<<"bad">>, session, [], <<"test">>),
%%     stop_mecking(),
%%     ok.

%% revoke_credential_test() ->
%%     CredId = <<"good_cred">>,
%%     BadId = <<"bad">>,
%%     setup_mecking(),
%%     {ok, _, []} = tts:revoke_credential_for(CredId, session),
%%     {error, _, []} = tts:revoke_credential_for(BadId, session),
%%     stop_mecking(),
%%     ok.



%% setup_mecking() ->
%%     MeckModules = [tts_session, tts_credential, tts_temp_cred, oidcc,
%%                    tts_service],
%%     Uid = <<"user1">>,
%%     CredId = <<"cred1">>,
%%     Issuer = <<"https://tts.issuer">>,
%%     Subject = <<"TTsUserOne">>,
%%     ServiceId = <<"good">>,
%%     Credential = [#{name => id, value=> CredId},
%%                   #{name => <<"user">>, type => text, value => <<"joe">>},
%%                   #{name => <<"password">>, type => text, value => <<"secret">>}
%%                  ],
%%     CredentialId = <<"good_cred">>,
%%     SessionId = <<"sess1">>,
%%     Token = #{access => #{ token => <<"AccessToken">>}},
%%     DisplayName = <<"User Name">>,

%%     GetUserInfo = fun(_SessionPid) ->
%%                           {ok, #{ site => #{uid => Uid}}}
%%                   end,
%%     GetToken = fun(_SessionPid) ->
%%                        {ok, Token}
%%                end,
%%     GetId = fun(_SessionPid) ->
%%                        {ok, SessionId}
%%                end,
%%     GetIssSub = fun(_SessionPid) ->
%%                        {ok, Issuer, Subject}
%%                end,
%%     GetDisplayName = fun(_SessionPid) ->
%%                              {ok, DisplayName}
%%                      end,
%%     CredentialExist = fun(UId, CId) ->
%%                               case {UId, CId} of
%%                                   {Uid, CredId} ->
%%                                       true;
%%                                   _ -> false
%%                               end
%%                       end,
%%     CredentialGetList = fun(_UId) ->
%%                                 {ok, [CredId]}
%%                            end,
%%     CredentialRequest = fun(SerId, _UserInfo, _IFace, _Token, _Param) ->
%%                                 case SerId of
%%                                     ServiceId ->
%%                                         {ok, Credential, []};
%%                                     _ ->
%%                                         {error, internal, []}
%%                                 end
%%                            end,

%%     CredentialRevoke = fun(CId, _UserInfo) ->
%%                                 case CId of
%%                                     CredentialId ->
%%                                         {ok, <<"done">>, []};
%%                                     _ ->
%%                                         {error, internal, []}
%%                                 end
%%                            end,
%%     TempCredExist = fun(CId, UsrInfo) ->
%%                             #{ site := #{uid := UId}} = UsrInfo,
%%                               case {UId, CId} of
%%                                   {Uid, CredId} ->
%%                                       true;
%%                                   _ -> false
%%                               end
%%                       end,

%%     TempCredAdd = fun(_Cred, _Session) ->
%%                           {ok, CredId}
%%                       end,

%%     TempCredGet = fun(CId, _Session) ->
%%                           case CId of
%%                               CredId ->
%%                                   {ok, Credential};
%%                               _ ->
%%                                   {error, not_found}
%%                           end
%%                   end,
%%     ServiceGetList = fun(_Uid) ->
%%                              {ok, []}
%%                      end,

%%     ServiceEnabled = fun(_Uid) ->
%%                              true
%%                      end,
%%     ProviderList = fun() ->
%%                              {ok, [ {<<"id">>, pid1} ]}
%%                      end,

%%     ProviderInfo = fun(_Pid) ->
%%                              {ok, #{description => <<"info">>,
%%                                      ready => false,
%%                                      issuer => <<"https://tts.issuer">>}}
%%                      end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_session, get_user_info, GetUserInfo),
%%     ok = meck:expect(tts_session, get_token, GetToken),
%%     ok = meck:expect(tts_session, get_display_name, GetDisplayName),
%%     ok = meck:expect(tts_session, get_id, GetId),
%%     ok = meck:expect(tts_session, get_iss_sub, GetIssSub),
%%     ok = meck:expect(tts_credential, exists, CredentialExist),
%%     ok = meck:expect(tts_credential, get_list, CredentialGetList),
%%     ok = meck:expect(tts_credential, request, CredentialRequest),
%%     ok = meck:expect(tts_credential, revoke, CredentialRevoke),
%%     ok = meck:expect(tts_service, get_list, ServiceGetList),
%%     ok = meck:expect(tts_service, is_enabled, ServiceEnabled),
%%     ok = meck:expect(tts_temp_cred, exists, TempCredExist),
%%     ok = meck:expect(tts_temp_cred, add_cred, TempCredAdd),
%%     ok = meck:expect(tts_temp_cred, get_cred, TempCredGet),
%%     ok = meck:expect(oidcc, get_openid_provider_list, ProviderList),
%%     ok = meck:expect(oidcc, get_openid_provider_info, ProviderInfo),
%%     ok.

%% stop_mecking() ->
%%     MeckModules = [tts_session, tts_credential, tts_temp_cred, oidcc,
%%                    tts_service],
%%     ok = test_util:meck_done(MeckModules),
%%     ok.
