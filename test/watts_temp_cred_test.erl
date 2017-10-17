-module(watts_temp_cred_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

start_stop_test() ->
    {ok, Pid} = watts_temp_cred:start_link(),
    ok = watts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),

    Cred = [],
    {ok, Pid2} = watts_temp_cred_data:start_link(Cred),
    ok = watts_temp_cred_data:stop(Pid2),
    ok = test_util:wait_for_process_to_die(Pid2,300),
    ?SETCONFIG( credential_timeout, 30000),
    ok.

add_get_test() ->
    MeckModules = [watts_temp_cred_sup],
    StartNew = fun(Cred) ->
                       watts_temp_cred_data:start(Cred)
               end,
    test_util:meck_new(MeckModules),
    meck:expect(watts_temp_cred_sup, new_temp_cred, StartNew),
    {ok, Pid} = watts_temp_cred:start_link(),

    UserInfo = #{ site => #{ uid => <<"user1">>}},
    {ok, Id} = watts_temp_cred:add_cred(#{}, UserInfo),

    {ok, List} = watts_temp_cred:get_all_creds(),
    ?assertEqual(1, length(List)),
    ?assertEqual(true, watts_temp_cred:exists(Id, UserInfo)),
    ?assertEqual({ok, #{}}, watts_temp_cred:get_cred(Id, UserInfo)),
    ?assertEqual(false, watts_temp_cred:exists(Id, UserInfo)),
    ?assertEqual({error, not_found},watts_temp_cred:get_cred(Id, UserInfo)),

    ?assertEqual({ok, []}, watts_temp_cred:get_all_creds()),

    ok = watts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok = test_util:meck_done(MeckModules),
    ok.


timeout_test() ->
    Cred = [],
    ?SETCONFIG( credential_timeout, 300),
    {ok, Pid} = watts_temp_cred_data:start_link(Cred),
    ok = test_util:wait_for_process_to_die(Pid, 500),
    ?SETCONFIG( credential_timeout, 10000),
    ok.


code_change_test() ->
    {ok, state} = watts_temp_cred_data:code_change(old, state, extra),
    {ok, state} = watts_temp_cred:code_change(old, state, extra),
    ok.
