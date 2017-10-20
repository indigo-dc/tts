-module(watts_jwt_keys_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

start_stop_test() ->
    {ok, Meck} = start_meck(),
    try
        update_info_file(),
        {ok, Pid} = watts_jwt_keys:start_link(),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

read_test() ->
    {ok, Meck} = start_meck(),
    try
        update_info_file(),
        {ok, Pid} = watts_jwt_keys:start_link(),
        ok = watts_jwt_keys:initial_read(),
        1 = length(?CONFIG(jwt_keys)),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

generation_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_jwt_keys:start_link(),
        ok = watts_jwt_keys:initial_read(),
        1 = length(?CONFIG(jwt_keys)),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

generation_due_old_test() ->
    {ok, Meck} = start_meck(),
    try
        old_info_file(),
        {ok, Pid} = watts_jwt_keys:start_link(),
        ok = watts_jwt_keys:initial_read(),
        1 = length(?CONFIG(jwt_keys)),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

autorotation_test() ->
    {ok, Meck} = start_meck(),
    try
        update_info_file(),
        Keys = [#{d => <<"SG288fWPcRwxxV9vF12f5LicxFIu2St5czmkgZFLuyIQ">>,
                  e => <<"AQAB">>, kty => <<"RSA">>,
                  n => <<"u-p877A8EH7VVDXhVz7y2Up773Me46PDyVv9umkcUA_w">>}
               ],
        ?SETCONFIG(jwt_keys, Keys),

        ?SETCONFIG( jwt_keys_started, true),
        {ok, Pid} = watts_jwt_keys:start_link(),
        Check = fun() ->
                        2 == length(?CONFIG(jwt_keys))
                end,
        ok = test_util:wait_for_true(Check, 200),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

autodelete_test_() ->
    {timeout, 30, ?_assertEqual(ok, run_autodelete())}.

run_autodelete() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG(session_timeout, 2000),
        ?SETCONFIG(jwt_key_rotation_interval, 1),
        update_info_file(),
        {ok, Pid} = watts_jwt_keys:start_link(),
        ok = watts_jwt_keys:initial_read(),
        Check1 = fun() ->
                        1 == length(?CONFIG(jwt_keys))
                end,
        ok = test_util:wait_for_true(Check1, 200),
        Check2 = fun() ->
                        2 == length(?CONFIG(jwt_keys))
                end,
        ok = test_util:wait_for_true(Check2, 200),
        timer:sleep(2000),
        ok = test_util:wait_for_true(Check2, 200),
        ok = watts_jwt_keys:stop(),
        test_util:wait_for_process_to_die(Pid, 300)
    after
        ok = stop_meck(Meck)
    end,
    ok.

current_time() ->
    erlang:system_time(seconds).

update_info_file() ->
    Info = #{generated => current_time(),
             bits => <<"2048">>},
    ok = file:write_file(info_file(), term_to_binary(Info)),
    ok.

old_info_file() ->
    Info = #{generated => 10,
             bits => <<"2048">>},
    ok = file:write_file(info_file(), term_to_binary(Info)),
    ok.

delete_info_file() ->
    file:delete(info_file()),
    ok.

info_file() ->
    KeyDir = ?CONFIG( secret_dir),
    filename:join(KeyDir, <<"jwt-key.info">>).


start_meck() ->
    init_config(),
    MeckModules = [exec],

    ExecRun = fun(Cmd, _Params) ->
                      io:format("Mocked: would run now ~p~n", [Cmd]),
                      {ok, not_checked}
              end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(exec, run, ExecRun),
    {ok, {MeckModules}}.

stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ?UNSETCONFIG( secret_dir ),
    ?UNSETCONFIG( jwt_key_rotation_interval ),
    ?UNSETCONFIG( jwt_keys_started),
    ok.

init_config() ->
    TestDir = filename:dirname(code:where_is_file("jwt.key")),
    ?SETCONFIG( secret_dir, TestDir),
    ?SETCONFIG(jwt_key_rotation_interval, 30),
    ?UNSETCONFIG(jwt_keys_started),
    ?UNSETCONFIG(jwt_keys),
    ?UNSETCONFIG(session_timeout),
    delete_info_file(),
    ok.
