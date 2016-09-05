-module(tts_temp_cred_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_temp_cred:start_link(),
    ok = tts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok.

add_get_test() ->
    {ok, Pid} = tts_temp_cred:start_link(),

    UserInfo = #{ site => #{ uid => <<"user1">>}},
    {ok, Id} = tts_temp_cred:add_cred(#{}, UserInfo),

    {ok, #{}} = tts_temp_cred:get_cred(Id, UserInfo),
    {error, _} = tts_temp_cred:get_cred(Id, UserInfo),


    ok = tts_temp_cred:stop(),
    ok = test_util:wait_for_process_to_die(Pid,300),
    ok.

