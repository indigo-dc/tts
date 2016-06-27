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

-define(USER,tts_user).
-define(MAPPING,tts_user_mapping).

user_test_() ->
    {timeout, 30, ?_assertEqual(ok, user_check())}.

user_check() ->
    %% testing functions related to user information caching
    %% functions to test:
    %% user_lookup_info/2
    %% user_insert_info/2
    %% user_delete_info/1
    %% user_delete_info/2
    %% user_delete_entries_older_than/1
    %% user_delete_entries_not_accessed_for/1
    %% user_clear_cache/0
    Issuer = <<"indigo">>,
    Subject1 = <<"joe">>,
    Subject2 = <<"alice">>,
    Subject3 = <<"john">>,

    User1 = #{ site => #{uid => 1, userIds => [{Issuer, Subject1}]}},
    User2 = #{ site => #{uid => 2, userIds => [{Issuer, Subject2}]}},
    User3 = #{ site => #{uid => 3, userIds => [{Issuer, Subject3}]}},

    MaxEntries = 2,
    {error, _} = tts_data:user_lookup_info(Issuer, Subject1),
    io:format("01: inserting ~p~n",[User1]),
    ok = tts_data:user_insert_info(User1, MaxEntries),
    display(?USER),
    display(?MAPPING),
    io:format("02: looking up ~p ~p~n",[Issuer, Subject1]),
    {ok, _} = tts_data:user_lookup_info(Issuer, Subject1),
    display(?USER),
    display(?MAPPING),
    io:format("03: inserting ~p~n",[User1]),
    {error, _} = tts_data:user_insert_info(User1, MaxEntries),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(1, ets:info(?USER, size)),
    ?assertEqual(1, ets:info(?MAPPING, size)),
    io:format("04: deleting ~p~n",[1]),
    ok = tts_data:user_delete_info(1),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(0, ets:info(?USER, size)),
    ?assertEqual(0, ets:info(?MAPPING, size)),
    false = ets:member(?USER, 1),
    io:format("05: inserting ~p~n",[User1]),
    ok = tts_data:user_insert_info(User1, MaxEntries),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(1, ets:info(?USER, size)),
    ?assertEqual(1, ets:info(?MAPPING, size)),
    io:format("06: deleting ~p ~p~n",[Issuer, Subject1]),
    ok = tts_data:user_delete_info(Issuer, Subject1),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(0, ets:info(?USER, size)),
    ?assertEqual(0, ets:info(?MAPPING, size)),


    io:format("07: inserting ~p ~p ~n",[User1,User2]),
    ok = tts_data:user_insert_info(User1, MaxEntries),
    sleep(1),
    ok = tts_data:user_insert_info(User2, MaxEntries),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(2, ets:info(?USER, size)),
    io:format("08: adding ~p ~n",[User3]),
    ok = tts_data:user_insert_info(User3, MaxEntries),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(2, ets:info(?USER, size)),
    io:format("09: looking up ~p ~p ~n",[Issuer, Subject1]),
    {error, _} = tts_data:user_lookup_info(Issuer, Subject1),
    io:format("10: looking up ~p ~n",[User1]),
    {error, _} = tts_data:user_delete_info(User1),

    io:format("11: deleting old entries ~n"),
    {ok, 2} = tts_data:user_delete_entries_older_than(5),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(0, ets:info(?USER, size)),
    ok = tts_data:user_clear_cache(),

    io:format("12: adding user1, user2 ~n"),
    ok = tts_data:user_insert_info(User1, MaxEntries),
    ok = tts_data:user_insert_info(User2, MaxEntries),
    display(?USER),
    display(?MAPPING),
    io:format("12: deleting ~p, ~p ~n",[User1, [Issuer, Subject2]]),
    ok = tts_data:user_delete_info(User1),
    ok = tts_data:user_delete_info(Issuer, Subject2),
    display(?USER),
    display(?MAPPING),
    ?assertEqual(0, ets:info(?USER, size)),
    ?assertEqual(0, ets:info(?MAPPING, size)),
    ok = tts_data:user_clear_cache(),


    io:format("13: inserting user2, user3 ~n"),
    ok = tts_data:user_insert_info(User2, MaxEntries),
    ok = tts_data:user_insert_info(User3, MaxEntries),
    sleep(1),
    display(?USER),
    display(?MAPPING),
    {ok, 2} = tts_data:user_delete_entries_not_accessed_for(1),
    ?assertEqual(0, ets:info(?USER, size)),
    ?assertEqual(0, ets:info(?MAPPING, size)),
    ok.

service_test() ->
    %% testing service handling related functions:
    %% service_add/2
    %% service_update/2
    %% service_get/1
    %% service_get_list/0
    ID1 = 1234,
    Config1 = #{id => ID1},
    Config1_1 = #{id => ID1, info=> some_info},
    ID2 = 5678,
    Config2 = #{id => ID2},
    ?assertEqual({ok, []}, tts_data:service_get_list()),
    ?assertEqual(ok, tts_data:service_add(ID1, Config1)),
    {ok, {ID1, #{id := ID1} = Info1}} = tts_data:service_get(ID1),
    ?assertEqual(undefined, maps:get(info, Info1, undefined)),
    {error, not_found} = tts_data:service_get(ID2),
    {ok, [#{id := ID1}]}  = tts_data:service_get_list(),
    ?assertEqual(ok, tts_data:service_add(ID2, Config2)),
    {ok, List}  = tts_data:service_get_list(),
    ?assertEqual(2, length(List)),
    ok = tts_data:service_update(ID1, Config1_1),
    {ok, {ID1, #{id := ID1} = Info2}} = tts_data:service_get(ID1),
    ?assertEqual(some_info, maps:get(info, Info2, undefined)),
    ok.


sleep(Secs) ->
    MiliSecs = Secs * 1000,
    receive
    after MiliSecs ->
              ok
    end.


display(Table) ->
    ets:safe_fixtable(Table, true),
    io:format("~n********************~n"),
    io:format("Table: ~p~n",[Table]),
    First = ets:first(Table),
    display(First, Table).


display('$end_of_table', Table) ->
    ets:safe_fixtable(Table, false),
    io:format("********************~n"),
    ok;
display(Current, Table) ->
    [Data] = ets:lookup(Table, Current),
    io:format("   ~p~n",[Data]),
    Next = ets:next(Table, Current),
    display(Next, Table).

