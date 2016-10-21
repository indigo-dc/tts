-module(tts_data_test).
-include_lib("eunit/include/eunit.hrl").


init_test() ->
    ok = tts_data:init(),
    ok.


session_test() ->
    %% functions to test:
    %%            sessions_get_list/0,
    %%            sessions_create_new/1,
    %%            sessions_get_pid/1,
    %%            sessions_update_pid/2,
    %%            sessions_delete/1
    ID1 = 1234,
    ID2 = 5678,
    ?assertEqual([], tts_data:sessions_get_list()),
    ok = tts_data:sessions_create_new(ID1),
    {error, _} = tts_data:sessions_create_new(ID1),
    [#{id := ID1}] = tts_data:sessions_get_list(),
    ok = tts_data:sessions_create_new(ID2),
    List = tts_data:sessions_get_list(),
    ?assertEqual(2,length(List)),
    Find = fun(#{id := Id}, IdList) ->
                   case lists:member(Id, IdList) of
                       true -> lists:delete(Id, IdList);
                       _ -> {missing, Id}
                   end
           end,
    ?assertEqual([], lists:foldl(Find, [ID1, ID2], List)),

    {error, _} = tts_data:sessions_get_pid(ID1),
    ?assertEqual(ok, tts_data:sessions_update_pid(ID1, self())),
    ?assertEqual({ok, self()}, tts_data:sessions_get_pid(ID1)),
    ?assertEqual(ok, tts_data:sessions_delete(ID1)),
    [#{id := ID2}] = tts_data:sessions_get_list(),
    ?assertEqual(ok, tts_data:sessions_delete(ID1)),
    ?assertEqual(ok, tts_data:sessions_delete(ID2)),
    ?assertEqual([], tts_data:sessions_get_list()),
    ok.
