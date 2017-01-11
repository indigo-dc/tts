-module(watts_data_sqlite_test).
-include_lib("eunit/include/eunit.hrl").
-define(TEST_DB,"tts_unittest.db").
-include("tts.hrl").

start_link_test() ->
    ?UNSETCONFIG(sqlite_db),
    {ok, Pid} = watts_data_sqlite:start_link(),
    ok = watts_data_sqlite:reconfigure(),
    {error, not_configured} = watts_data_sqlite:credential_add(<<"user1">>,
                                                             <<"service1">>,
                                                             <<"if1">>,
                                                             <<"state1">>,
                                                             false),
    ok = watts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    ok.


create_db_test_() ->
    {timeout, 30, ?_assertEqual(ok, create_db_check())}.

create_db_check() ->
    file:delete(?TEST_DB),
    ?SETCONFIG( sqlite_db, ?TEST_DB),
    {ok, Pid} = watts_data_sqlite:start_link(),
    ok = watts_data_sqlite:reconfigure(),
    ok = watts_data_sqlite:reconfigure(),
    ok = watts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    file:delete(?TEST_DB),
    ok.

credential_test_() ->
    {timeout, 30, ?_assertEqual(ok, credential_check())}.

credential_check() ->
    file:delete(?TEST_DB),
    ?SETCONFIG(sqlite_db,?TEST_DB),
    {ok, Pid} = watts_data_sqlite:start_link(),
    ok = watts_data_sqlite:reconfigure(),
    MockModules = [cowboy_clock],
    test_util:meck_new(MockModules),
    meck:expect(cowboy_clock, rfc1123, fun() -> <<"today">> end),

    CredState = <<"cstate">>,
    {ok, CredId} = watts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  false),
    {ok, #{cred_state := CredState}} = watts_data_sqlite:credential_get(CredId),
    {error, _} = watts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  false),

    {ok, CredId} = watts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  true),
    {error, _} = watts_data_sqlite:credential_get(<<"123">>),
    {ok, List} = watts_data_sqlite:credential_get_list(<<"user1">>),
    ?assertEqual(1,length(List)),
    {ok, Count} = watts_data_sqlite:credential_get_count(<<"user1">>,
                                                       <<"service1">>),
    ?assertEqual(1,Count),
    {ok, Count2} = watts_data_sqlite:credential_get_count(<<"user2">>,
                                                       <<"service1">>),
    ?assertEqual(0,Count2),
    {ok, []} = watts_data_sqlite:credential_get_list(<<"user2">>),

    ok = watts_data_sqlite:credential_remove(<<"user1">>, CredId),

    {ok, Count3} = watts_data_sqlite:credential_get_count(<<"user1">>,
                                                       <<"service1">>),
    ?assertEqual(0,Count3),
    ok = watts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    ok = test_util:meck_done(MockModules),

    file:delete(?TEST_DB),
    ok.
