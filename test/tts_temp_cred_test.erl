-module(tts_temp_cred_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_temp_cred:start_link(),
    ok = tts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),

    Cred = [],
    {ok, Pid2} = tts_temp_cred_data:start_link(Cred),
    ok = tts_temp_cred_data:stop(Pid2),
    ok = test_util:wait_for_process_to_die(Pid2,300),
    application:set_env(tts, credential_timeout, 30000),
    ok.

add_get_test() ->
    MeckModules = [tts_temp_cred_sup],
    StartNew = fun(Cred) ->
                       tts_temp_cred_data:start(Cred)
               end,
    test_util:meck_new(MeckModules),
    meck:expect(tts_temp_cred_sup, new_temp_cred, StartNew),
    {ok, Pid} = tts_temp_cred:start_link(),

    UserInfo = #{ site => #{ uid => <<"user1">>}},
    {ok, Id} = tts_temp_cred:add_cred(#{}, UserInfo),

    {ok, List} = tts_temp_cred:get_all_creds(),
    ?assertEqual(1, length(List)),
    ?assertEqual(true, tts_temp_cred:exists(Id, UserInfo)),
    ?assertEqual({ok, #{}}, tts_temp_cred:get_cred(Id, UserInfo)),
    ?assertEqual(false, tts_temp_cred:exists(Id, UserInfo)),
    ?assertEqual({error, not_found},tts_temp_cred:get_cred(Id, UserInfo)),

    ?assertEqual({ok, []}, tts_temp_cred:get_all_creds()),

    ok = tts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok = test_util:meck_done(MeckModules),
    ok.


timeout_test() ->
    Cred = [],
    application:set_env(tts, credential_timeout, 300),
    {ok, Pid} = tts_temp_cred_data:start_link(Cred),
    ok = test_util:wait_for_process_to_die(Pid, 500),
    application:set_env(tts, credential_timeout, 10000),
    ok.
