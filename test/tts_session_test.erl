-module(tts_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(ID,<<"JustSomeID">>).

start_stop_id_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    {ok, Pid} = tts_session:start_link(?ID),
    {ok, ?ID} = tts_session:get_id(Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).

get_set_max_age_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    {ok, Pid} = tts_session:start_link(?ID),
    %set up the mocking of tts_session_mgr
    CloseFun = fun(ID) ->
                       ID = ?ID,
                       tts_session:close(Pid)
               end,
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    ok = meck:expect(tts_session_mgr,session_wants_to_close, CloseFun),
    {ok, _} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1000,Pid),
    {ok, 1000} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1,Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).

oidc_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
    {ok, Pid} = tts_session:start_link(?ID),
    {ok, OidcState} = tts_session:get_oidc_state(Pid),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Pid),
    true = is_binary(OidcState),
    true = is_binary(OidcNonce),
    true = tts_session:is_oidc_state(OidcState,Pid),
    true = tts_session:is_oidc_nonce(OidcNonce,Pid),
    false = tts_session:is_oidc_state(OidcNonce,Pid),
    false = tts_session:is_oidc_nonce(OidcState,Pid),
    ok = tts_session:clear_oidc_state_nonce(Pid),
    {ok, cleared} = tts_session:get_oidc_state(Pid),
    {ok, cleared} = tts_session:get_oidc_nonce(Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).

token_login_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
    {ok, Pid} = tts_session:start_link(?ID),
    Subject = "foo",
    Issuer = "https://bar.com",
    Token = #{id => #{ claims => #{sub => Subject, iss=>Issuer}}},
    false = tts_session:is_logged_in(Pid),
    ok = tts_session:set_token(Token, Pid),
    true = tts_session:is_logged_in(Pid),
    {ok, Token} = tts_session:get_token(Pid),
    true = tts_session:is_logged_in(Pid),
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Pid),
    true = tts_session:is_logged_in(Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).



redirect_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
    Redirect = <<"https://localhost/back">>,
    BadRedirect = "https://localhost/back",
    {ok, Pid} = tts_session:start_link(?ID),
    ok = tts_session:set_used_redirect(Redirect, Pid),
    {ok,Redirect} = tts_session:get_used_redirect(Pid),
    ignored = tts_session:set_used_redirect(BadRedirect, Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).



oidc_provider_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    {ok, Pid} = tts_session:start_link(?ID),
    OidcId = <<"SomeOpenIdProviderId">>,
    ok = tts_session:set_oidc_provider(OidcId, Pid),
    {ok, OidcId} = tts_session:get_oidc_provider(Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).



same_ua_ip_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    IP = {123,123,123,123},
    UA = <<"some browser">>,
    {ok, Pid} = tts_session:start_link(?ID),
    true = tts_session:is_user_agent(UA,Pid),
    false = tts_session:is_user_agent(IP,Pid),
    true = tts_session:is_user_agent(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    false = tts_session:is_same_ip(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).


garbage_test() ->
    MeckModules = [tts_session_mgr],
    test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    {ok, Pid} = tts_session:start_link(?ID),
    Pid ! <<"some data">>,
    ok = gen_server:cast(Pid,some_cast),
    still_alive = test_util:wait_for_process_to_die(Pid,5),
    ok = tts_session:close(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules).

