-module(tts_idh_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_idh:start_link(),

    {error, not_configured} = tts_idh:lookup_user(#{}),
    ok = tts_idh:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 300),
    ok.


idh_test() ->
    MeckModules = [tts_idh_worker_sup],
    UserMap = #{type => openidconnect, subject => <<"sub">>, issuer =>
                <<"iss">>},
    StartWorker = fun() ->
                          tts_idh_worker:start_link()
                  end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_idh_worker_sup, start_worker, StartWorker),


    application:unset_env(tts, idh_script),
    {ok, Pid} = tts_idh:start_link(),
    ?assertEqual({error, not_configured}, tts_idh:lookup_user(#{})),
    application:set_env(tts, idh_script, "./test/mock_idh.py"),
    ok = tts_idh:reconfigure(),
    {ok, _} = tts_idh:lookup_user(UserMap),
    ok = tts_idh:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 300),
    ok = test_util:meck_done(MeckModules),
    ok.
