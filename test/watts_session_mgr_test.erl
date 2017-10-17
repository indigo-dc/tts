-module(watts_session_mgr_test).
-include_lib("eunit/include/eunit.hrl").


start_stop_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_session_mgr:start_link(),
        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.



session_start_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_session_mgr:start_link(),
        {ok, Session} = watts_session_mgr:new_session(),

        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Session, 100),
        ok = test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.


session_timeout_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_session_mgr:start_link(),
        {ok, Session} = watts_session_mgr:new_session(),

        {ok, Id} =  watts_session:get_sess_token(Session),
        {ok, Session} = watts_session_mgr:get_session(Id),
        watts_session:set_max_age(1,Session),
        ok = test_util:wait_for_process_to_die(Session, 100),

        {ok, undefined} = watts_session_mgr:get_session(Id),
        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.


session_lookup_stop_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_session_mgr:start_link(),
        {ok, Session} = watts_session_mgr:new_session(),
        {ok, ID} = watts_session:get_sess_token(Session),

        %% Lookup an existing session
        {ok, Session} = watts_session_mgr:get_session(ID),
        ok = watts_session:close(Session),
        ok = test_util:wait_for_process_to_die(Session, 100),

        %% try to lookup the closed session, yet fail and return undefined
        {ok, undefined} = watts_session_mgr:get_session(ID),

        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.


close_all_test() ->
    {ok, Meck} = start_meck(),
    try
        NumberOfSessions = 20,
        {ok, Pid} = watts_session_mgr:start_link(),
        {ok, Sessions} = start_k_sessions(NumberOfSessions, []),
        AllSessionsStopped
            = fun(P, Result) ->
                      case Result of
                          false ->
                              false;
                          true ->
                              not test_util:session_running(P)
                      end
              end,
        Check = fun() ->
                        lists:foldl(AllSessionsStopped, true, Sessions)
                end,
        ok = watts_session_mgr:close_all_sessions(),
        ok = test_util:wait_for_true(Check, 100),
        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Pid, 100)
    after
        ok = stop_meck(Meck)
    end,
    ok.

start_k_sessions(0, List) ->
    {ok, List};
start_k_sessions(K, List) ->
    {ok, Pid} = watts_session_mgr:new_session(),
    start_k_sessions(K - 1, [Pid | List] ).




garbage_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, Pid} = watts_session_mgr:start_link(),
        Pid ! <<"some data">>,
        ok = gen_server:cast(Pid,some_cast),
        ignored = gen_server:call(Pid,some_unsupported_call),
        still_alive = test_util:wait_for_process_to_die(Pid,5),
        ok = watts_session_mgr:stop(),
        ok = test_util:wait_for_process_to_die(Pid,100)
    after
        ok = stop_meck(Meck)
    end,
    ok.


start_meck() ->
    MeckModules = [watts_session_sup],
    watts_ets:destroy(),
    watts_ets:init(),
    ok = test_util:meck_new(MeckModules),
    NewSession = fun(ID) ->
                         watts_session:start_link(ID)
                 end,
    ok = meck:expect(watts_session_sup, new_session,NewSession),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    watts_ets:destroy(),
    ok = test_util:meck_done(MeckModules),
    ok.
