-module(watts_utils_test).
-include_lib("eunit/include/eunit.hrl").

random_string_test() ->
    MeckModules = [crypto],
    Start = erlang:system_time(milli_seconds),
    RandomBytes = fun(_Length) ->
                          Now = erlang:system_time(milli_seconds),
                          case Now > (Start + 300) of
                              true -> <<"abcdefghijkl">>;
                              false -> throw(low_entropy)
                          end
                  end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(crypto, strong_rand_bytes, RandomBytes),
    watts_utils:random_string(10),
    %% just unload as this is known and wanted to fail
    ok = meck:unload(crypto),
    ok.


lists_join_test() ->
    ?assertEqual([], watts_utils:lists_join(x, [])),
    ?assertEqual([a], watts_utils:lists_join(x, [a])),
    ?assertEqual([a,x,b,x,c,x,d], watts_utils:lists_join(x, [a,b,c,d])),
    ok.
