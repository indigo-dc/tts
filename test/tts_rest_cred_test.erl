-module(tts_rest_cred_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_rest_cred:start_link(),
    ok = tts_rest_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok.

add_get_test() ->
    {ok, Pid} = tts_rest_cred:start_link(),

    UserId = <<"user1">>,
    {ok, Id} = tts_rest_cred:add_cred(#{}, UserId),

    {ok, #{}} = tts_rest_cred:get_cred(Id, UserId),
    {error, _} = tts_rest_cred:get_cred(Id, UserId),


    ok = tts_rest_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok.

