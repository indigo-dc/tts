-module(tts_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN,<<"JustSomeSessionToken">>).

start_stop_id_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {ok, ?TOKEN} = tts_session:get_sess_token(Pid),
    {ok, Pid} = tts_session:get_id(Pid),
    ok = tts_session:close(Pid),
    ok = stop_meck(Meck).

undefined_test() ->
    {ok, undefined} = tts_session:get_id(undefined),
    {ok, undefined} = tts_session:get_sess_token(undefined),
    {ok, state} = tts_session:code_change(old, state, extra),
    ok.


get_set_max_age_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {ok, _} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1000,Pid),
    {ok, 1000} = tts_session:get_max_age(Pid),
    ok = tts_session:set_max_age(1,Pid),
    ok = stop_meck(Meck).

userinfo_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {error, not_set} = tts_session:get_userid(Pid),
    {error, not_set} = tts_session:get_display_name(Pid),
    false = tts_session:is_logged_in(Pid),
    {ok, Info} = tts_session:get_user_info(Pid),
    false = tts_userinfo:return(logged_in, Info),
    {error, not_set} = tts_userinfo:return(id, Info),
    {error, not_set} = tts_userinfo:return(display_name, Info),


    ok = tts_session:set_iss_sub(<<"iss">>, <<"sub">>, Pid),
    {ok, _} = tts_session:get_userid(Pid),
    {ok, _} = tts_session:get_display_name(Pid),

    IdToken = #{claims => #{iss => <<"iss">>, sub => <<"sub">>}},
    InfoToken = #{sub => <<"sub">>},
    AccessToken = #{token => <<"ajakljeeeoasn">>},
    ok = tts_session:set_token(#{id => IdToken,
                                 user_info => InfoToken,
                                 access => AccessToken}, Pid),
    ok = tts_session:close(Pid),
    ok = stop_meck(Meck).

%% token_login_test() ->
%%     MeckModules = [tts_session_mgr],
%%     test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
%%     {ok, Pid} = tts_session:start_link(?ID),
%%     Subject = "foo",
%%     Issuer = "https://bar.com",
%%     Token = #{id => #{ claims => #{sub => Subject, iss=>Issuer}}},
%%     false = tts_session:is_logged_in(Pid),
%%     ok = tts_session:set_token(Token, Pid),
%%     true = tts_session:is_logged_in(Pid),
%%     {ok, Token} = tts_session:get_token(Pid),
%%     true = tts_session:is_logged_in(Pid),
%%     {ok, Issuer, Subject} = tts_session:get_iss_sub(Pid),
%%     true = tts_session:is_logged_in(Pid),
%%     ok = tts_session:close(Pid),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules).



%% redirect_test() ->
%%     MeckModules = [tts_session_mgr],
%%     test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
%%     Redirect = <<"https://localhost/back">>,
%%     BadRedirect = "https://localhost/back",
%%     {ok, Pid} = tts_session:start_link(?ID),
%%     ok = tts_session:set_used_redirect(Redirect, Pid),
%%     {ok,Redirect} = tts_session:get_used_redirect(Pid),
%%     ignored = tts_session:set_used_redirect(BadRedirect, Pid),
%%     ok = tts_session:close(Pid),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules).



error_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    EMsg = <<"some error message">>,
    {ok, <<>>} = tts_session:get_error(Pid),
    ok = tts_session:set_error(EMsg, Pid),
    {ok, EMsg} = tts_session:get_error(Pid),
    ok = tts_session:close(Pid),
    ok = stop_meck(Meck).



same_ua_ip_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    IP = {123,123,123,123},
    UA = <<"some browser">>,
    true = tts_session:is_user_agent(UA,Pid),
    false = tts_session:is_user_agent(IP,Pid),
    true = tts_session:is_user_agent(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    false = tts_session:is_same_ip(UA,Pid),
    true = tts_session:is_same_ip(IP,Pid),
    ok = tts_session:close(Pid),
    ok = stop_meck(Meck).


garbage_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    Pid ! <<"some data">>,
    ignored = gen_server:call(Pid,some_call),
    ok = gen_server:cast(Pid,some_cast),
    still_alive = test_util:wait_for_process_to_die(Pid,5),
    ok = tts_session:close(Pid),
    ok = stop_meck(Meck).

start_meck() ->
    MeckModules = [tts_session_mgr],
    ok = test_util:meck_new(MeckModules),
    {ok, Pid} = tts_session:start_link(?TOKEN),
    CloseFun = fun(Token) ->
                       Token = ?TOKEN,
                       tts_session:close(Pid)
               end,
    ok = meck:expect(tts_session_mgr,session_terminating, fun(_) -> ok end),
    ok = meck:expect(tts_session_mgr,session_wants_to_close, CloseFun),
    {ok, {MeckModules, Pid}}.


stop_meck({MeckModules, Pid}) ->
    ok = test_util:wait_for_process_to_die(Pid,10),
    ok = test_util:meck_done(MeckModules),
    ok.
