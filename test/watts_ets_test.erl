-module(watts_ets_test).
-include_lib("eunit/include/eunit.hrl").


session_test() ->
    %% functions to test:
    %%            sessions_get_list/0,
    %%            sessions_create_new/1,
    %%            sessions_get_pid/1,
    %%            sessions_update_pid/2,
    %%            sessions_delete/1
    {ok, _} = watts_ets:start_link(),
    ok = watts_ets:init(),
    ID1 = 1234,
    ID2 = 5678,
    ?assertEqual([], watts_ets:sessions_get_list()),
    ok = watts_ets:sessions_create_new(ID1),
    {error, _} = watts_ets:sessions_create_new(ID1),
    [#{id := ID1}] = watts_ets:sessions_get_list(),
    ok = watts_ets:sessions_create_new(ID2),
    List = watts_ets:sessions_get_list(),
    ?assertEqual(2,length(List)),
    Find = fun(#{id := Id}, IdList) ->
                   case lists:member(Id, IdList) of
                       true -> lists:delete(Id, IdList);
                       _ -> {missing, Id}
                   end
           end,
    ?assertEqual([], lists:foldl(Find, [ID1, ID2], List)),

    {error, _} = watts_ets:sessions_get_pid(ID1),
    ?assertEqual(ok, watts_ets:sessions_update_pid(ID1, self())),
    ?assertEqual({ok, self()}, watts_ets:sessions_get_pid(ID1)),
    ?assertEqual(ok, watts_ets:sessions_delete(ID1)),
    [#{id := ID2}] = watts_ets:sessions_get_list(),
    ?assertEqual(ok, watts_ets:sessions_delete(ID1)),
    ?assertEqual(ok, watts_ets:sessions_delete(ID2)),
    ?assertEqual([], watts_ets:sessions_get_list()),
    ok = watts_ets:destroy(),
    ok = watts_ets:stop(),
    ok.

service_test() ->
    %% functions to test:
    %%     service_add/2,
    %%     service_update/2,
    %%     service_get/1,
    %%     service_get_list/0
    {ok, _} = watts_ets:start_link(),
    ok = watts_ets:init(),
    ID1 = 1234,
    ID2 = 5678,
    ?assertEqual({ok, []}, watts_ets:service_get_list()),
    ok = watts_ets:service_add(ID1, #{info => nothing, id => ID1}),
    {error, _} = watts_ets:service_add(ID1, #{}),
    ?assertEqual({ok, [#{info => nothing, id => ID1}]}, watts_ets:service_get_list()),
    ?assertEqual({ok, {ID1, #{info => nothing, id => ID1}}}, watts_ets:service_get(ID1)),
    ok = watts_ets:service_update(ID1, #{id => ID1, info => important}),
    ?assertEqual({ok, [#{info => important, id => ID1}]}, watts_ets:service_get_list()),
    ok = watts_ets:service_add(ID2, #{id => ID2}),
    {ok, List} = watts_ets:service_get_list(),
    ?assertEqual(2,length(List)),
    Find = fun(#{id := Id}, IdList) ->
                   case lists:member(Id, IdList) of
                       true -> lists:delete(Id, IdList);
                       _ -> {missing, Id}
                   end
           end,
    ?assertEqual([], lists:foldl(Find, [ID1, ID2], List)),

    ok = watts_ets:destroy(),
    ok = watts_ets:stop(),
    ok.
