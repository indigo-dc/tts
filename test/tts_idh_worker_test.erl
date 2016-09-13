-module(tts_idh_worker_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_idh_worker:start_link(),
    ok = tts_idh_worker:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok.


lookup_test_() ->
    {timeout, 30, ?_assertEqual(ok, lookup_check())}.

lookup_check() ->
    {ok, Pid} = tts_idh_worker:start_link(),

    MeckModules = [tts_idh],
    Issuer = <<"some">>,
    Subject = <<"me">>,
    ok = test_util:meck_new(MeckModules),
    Result = fun(Res) ->
                     ok = tts_idh_worker:stop(Pid),
                     #{gidNumber := 1000, homeDirectory := <<"/home/you">>,
                       uid := Sub, uidNumber := 1000,
                       userIds := [[Iss, Sub]]} = Res,
                     ?assertEqual(Issuer, Iss),
                     ?assertEqual(Subject, Sub),
                     ok
             end,
    ok = meck:expect(tts_idh, user_result, Result),


    Params = #{type => openidconnect, subject => Subject,
               issuer => Issuer},
    ok = tts_idh_worker:lookup("./test/mock_idh.py",Params, Pid),

    ok = test_util:wait_for_process_to_die(Pid,3000),
    ok = test_util:meck_done(MeckModules),
    ok.
