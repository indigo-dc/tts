-module(tts_data_sqlite_test).
-include_lib("eunit/include/eunit.hrl").

%% -export([start_link/0]).
%% -export([reconfigure/0]).
%% -export([credential_add/5]).
%% -export([credential_get/1]).
%% -export([credential_get_list/1]).
%% -export([credential_get_count/2]).
%% -export([credential_remove/2]).

-define(TEST_DB,"tts_unittest.db").

start_link_test() ->
    application:unset_env(tts,sqlite_db),
    {ok, Pid} = tts_data_sqlite:start_link(),
    ok = tts_data_sqlite:reconfigure(),
    {error, not_configured} = tts_data_sqlite:credential_add(<<"user1">>,
                                                             <<"service1">>,
                                                             <<"if1">>,
                                                             <<"state1">>,
                                                             false),
    ok = tts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    ok.


create_db_test() ->
    file:delete(?TEST_DB),
    application:set_env(tts,sqlite_db,?TEST_DB),
    {ok, Pid} = tts_data_sqlite:start_link(),
    ok = tts_data_sqlite:reconfigure(),
    ok = tts_data_sqlite:reconfigure(),
    ok = tts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    file:delete(?TEST_DB),
    ok.


credential_test() ->
    file:delete(?TEST_DB),
    application:set_env(tts,sqlite_db,?TEST_DB),
    {ok, Pid} = tts_data_sqlite:start_link(),
    ok = tts_data_sqlite:reconfigure(),

    CredState = <<"cstate">>,
    {ok, CredId} = tts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  false),
    {ok, #{cred_state := CredState}} = tts_data_sqlite:credential_get(CredId),
    {error, _} = tts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  false),

    {ok, CredId} = tts_data_sqlite:credential_add(<<"user1">>, <<"service1">>,
                                                  <<"if1">>, CredState,
                                                  true),
    {error, _} = tts_data_sqlite:credential_get(<<"123">>),
    {ok, List} = tts_data_sqlite:credential_get_list(<<"user1">>),
    ?assertEqual(1,length(List)),
    {ok, Count} = tts_data_sqlite:credential_get_count(<<"user1">>,
                                                       <<"service1">>),
    ?assertEqual(1,Count),
    {ok, Count2} = tts_data_sqlite:credential_get_count(<<"user2">>,
                                                       <<"service1">>),
    ?assertEqual(0,Count2),
    {ok, []} = tts_data_sqlite:credential_get_list(<<"user2">>),

    ok = tts_data_sqlite:credential_remove(<<"user1">>, CredId),

    {ok, Count3} = tts_data_sqlite:credential_get_count(<<"user1">>,
                                                       <<"service1">>),
    ?assertEqual(0,Count3),
    ok = tts_data_sqlite:stop(),
    ok = test_util:wait_for_process_to_die(Pid,1000),
    file:delete(?TEST_DB),
    ok.
