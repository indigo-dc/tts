-module(watts_session_test).
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN,<<"JustSomeSessionToken">>).

start_stop_id_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {ok, ?TOKEN} = watts_session:get_sess_token(Pid),
    {ok, Pid} = watts_session:get_id(Pid),
    ok = watts_session:close(Pid),
    ok = stop_meck(Meck).

undefined_test() ->
    {ok, undefined} = watts_session:get_id(undefined),
    {ok, undefined} = watts_session:get_sess_token(undefined),
    {ok, state} = watts_session:code_change(old, state, extra),
    ok.


get_set_max_age_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {ok, _} = watts_session:get_max_age(Pid),
    ok = watts_session:set_max_age(1000,Pid),
    {ok, 1000} = watts_session:get_max_age(Pid),
    ok = watts_session:set_max_age(1,Pid),
    ok = stop_meck(Meck).

userinfo_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    {error, not_set} = watts_session:get_userid(Pid),
    {error, not_set} = watts_session:get_display_name(Pid),
    false = watts_session:is_logged_in(Pid),
    {ok, Info} = watts_session:get_user_info(Pid),
    false = watts_userinfo:return(logged_in, Info),
    {error, not_set} = watts_userinfo:return(id, Info),
    {error, not_set} = watts_userinfo:return(display_name, Info),


    ok = watts_session:set_iss_sub(<<"iss">>, <<"sub">>, Pid),
    {ok, _} = watts_session:get_userid(Pid),
    {ok, _} = watts_session:get_display_name(Pid),

    IdToken = #{claims => #{iss => <<"iss">>, sub => <<"sub">>}},
    InfoToken = #{sub => <<"sub">>},
    AccessToken = #{token => <<"ajakljeeeoasn">>},
    ok = watts_session:set_token(#{id => IdToken,
                                 user_info => InfoToken,
                                 access => AccessToken}, Pid),
    ok = watts_session:close(Pid),
    ok = stop_meck(Meck).


error_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    EMsg = <<"some error message">>,
    {ok, <<>>} = watts_session:get_error(Pid),
    ok = watts_session:set_error(EMsg, Pid),
    {ok, EMsg} = watts_session:get_error(Pid),
    ok = watts_session:close(Pid),
    ok = stop_meck(Meck).



same_ua_ip_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    IP = {123,123,123,123},
    UA = <<"some browser">>,
    true = watts_session:is_user_agent(UA,Pid),
    false = watts_session:is_user_agent(IP,Pid),
    true = watts_session:is_user_agent(UA,Pid),
    true = watts_session:is_same_ip(IP,Pid),
    false = watts_session:is_same_ip(UA,Pid),
    true = watts_session:is_same_ip(IP,Pid),
    ok = watts_session:close(Pid),
    ok = stop_meck(Meck).


garbage_test() ->
    {ok, {_, Pid} = Meck} = start_meck(),
    Pid ! <<"some data">>,
    ignored = gen_server:call(Pid,some_call),
    ok = gen_server:cast(Pid,some_cast),
    still_alive = test_util:wait_for_process_to_die(Pid,5),
    ok = watts_session:close(Pid),
    ok = stop_meck(Meck).

start_meck() ->
    MeckModules = [watts_session_mgr],
    ok = test_util:meck_new(MeckModules),
    {ok, Pid} = watts_session:start_link(?TOKEN),
    CloseFun = fun(Token) ->
                       Token = ?TOKEN,
                       watts_session:close(Pid)
               end,
    ok = meck:expect(watts_session_mgr,session_terminating, fun(_) -> ok end),
    ok = meck:expect(watts_session_mgr,session_wants_to_close, CloseFun),
    {ok, {MeckModules, Pid}}.


stop_meck({MeckModules, Pid}) ->
    ok = test_util:wait_for_process_to_die(Pid,10),
    ok = test_util:meck_done(MeckModules),
    ok.
