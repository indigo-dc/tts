-module(tts_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(ID,<<"JustSomeID">>).

start_stop_linked_test() ->
    {ok, Pid} = tts_session:start_link(?ID),
    ok = tts_session:close(Pid).

start_stop_id_test() ->
    {ok, Pid} = tts_session:start(?ID),
    {ok, ?ID} = tts_session:get_id(Pid),
    ok = tts_session:close(Pid),
    try tts_session:get_id(Pid) of
        _ -> erlang:error(not_stopped)
    catch exit:_ ->
              ok
    end.

get_set_max_age_test() ->
    {ok, Pid} = tts_session:start(?ID),
    {ok, _} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1000,Pid),
    {ok, 1000} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1,Pid),
    % This will raise an error as tts_session_mgr does not exist
    % the cooperation between session and session_mgr
    % is part of common tests, not unit testing
    timer:sleep(5),
    try tts_session:get_id(Pid) of
        _ -> erlang:error(not_stopped)
    catch exit:_ ->
              ok
    end,
    ok.

oidc_test() ->
    {ok, Pid} = tts_session:start(?ID),
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
    ok = tts_session:close(Pid).

token_login_test() ->
    {ok, Pid} = tts_session:start(?ID),
    Subject = "foo",
    Issuer = "https://bar.com",
    Token = #{id => #{sub => Subject, iss=>Issuer}},
    BadToken = #{id => #{ iss=>Issuer}},
    false = tts_session:is_logged_in(Pid),
    ok = tts_session:set_token(Token, Pid),
    true = tts_session:is_logged_in(Pid),
    {ok, Token} = tts_session:get_token(Pid),
    true = tts_session:is_logged_in(Pid),
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Pid),
    true = tts_session:is_logged_in(Pid),
    ignored = tts_session:set_token(BadToken, Pid),
    ok = tts_session:close(Pid).


redirect_test() ->
    Redirect = <<"https://localhost/back">>,
    BadRedirect = "https://localhost/back",
    {ok, Pid} = tts_session:start(?ID),
    ok = tts_session:set_used_redirect(Redirect, Pid),
    {ok,Redirect} = tts_session:get_used_redirect(Pid),
    ignored = tts_session:set_used_redirect(BadRedirect, Pid),
    ok = tts_session:close(Pid).


oidc_provider_test() ->
    {ok, Pid} = tts_session:start(?ID),
    Issuer = "https://bar.com",
    OidcMap = #{some_info=>Issuer},
    ok = tts_session:set_oidc_provider(OidcMap, Pid),
    {ok, OidcMap} = tts_session:get_oidc_provider(Pid),
    ignored = tts_session:set_oidc_provider(<<>>, Pid),
    ok = tts_session:close(Pid).


same_ua_ip_test() ->
    IP = {123,123,123,123},
    UA = <<"some browser">>,
    {ok, Pid} = tts_session:start(?ID),
    true = tts_session:is_user_agent(UA,Pid),
    false = tts_session:is_user_agent(IP,Pid),
    true = tts_session:is_user_agent(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    false = tts_session:is_same_ip(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    ok = tts_session:close(Pid).

