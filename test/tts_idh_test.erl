-module(tts_idh_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_idh:start_link(),

    {error, not_configured} = tts_idh:lookup_user(#{}),
    ok = tts_idh:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 300),
    ok.


