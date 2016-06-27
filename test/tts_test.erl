-module(tts_test).
-include_lib("eunit/include/eunit.hrl").


debug_mode_test() ->
    tts:set_debug_mode(true),
    ?assertEqual({ok, true}, application:get_env(tts, debug_mode)),
    tts:set_debug_mode(false),
    ?assertEqual({ok, false}, application:get_env(tts, debug_mode)),
    ok.
