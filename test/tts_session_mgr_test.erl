-module(tts_session_mgr_test).
-include_lib("eunit/include/eunit.hrl").


start_stop_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_session_mgr:start_link(),
    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.



session_start_test() ->
    {ok, Meck} = start_meck(),

    {ok, Pid} = tts_session_mgr:start_link(),
    {ok, Session} = tts_session_mgr:new_session(),

    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Session, 100),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.


session_timeout_test() ->
    {ok, Meck} = start_meck(),

    {ok, Pid} = tts_session_mgr:start_link(),
    {ok, Session} = tts_session_mgr:new_session(),

    {ok, Id} =  tts_session:get_sess_token(Session),
    {ok, Session} = tts_session_mgr:get_session(Id),
    tts_session:set_max_age(1,Session),
    ok = test_util:wait_for_process_to_die(Session, 100),

    {ok, undefined} = tts_session_mgr:get_session(Id),
    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.


session_lookup_stop_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_session_mgr:start_link(),
    {ok, Session} = tts_session_mgr:new_session(),
    {ok, ID} = tts_session:get_sess_token(Session),

    %% Lookup an existing session
    {ok, Session} = tts_session_mgr:get_session(ID),
    ok = tts_session:close(Session),
    ok = test_util:wait_for_process_to_die(Session, 100),

    %% try to lookup the closed session, yet fail and return undefined
    {ok, undefined} = tts_session_mgr:get_session(ID),

    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 100),

    ok = stop_meck(Meck),
    ok.


close_all_test() ->
    {ok, Meck} = start_meck(),

    NumberOfSessions = 20,
    {ok, Pid} = tts_session_mgr:start_link(),
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
    ok = tts_session_mgr:close_all_sessions(),
    ok = test_util:wait_for_true(Check, 100),
    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok = stop_meck(Meck),
    ok.

start_k_sessions(0, List) ->
    {ok, List};
start_k_sessions(K, List) ->
    {ok, Pid} = tts_session_mgr:new_session(),
    start_k_sessions(K - 1, [Pid | List] ).




garbage_test() ->
    {ok, Meck} = start_meck(),
    {ok, Pid} = tts_session_mgr:start_link(),
    Pid ! <<"some data">>,
    ok = gen_server:cast(Pid,some_cast),
    ignored = gen_server:call(Pid,some_unsupported_call),
    still_alive = test_util:wait_for_process_to_die(Pid,5),
    ok = tts_session_mgr:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = stop_meck(Meck),
    ok.


start_meck() ->
    MeckModules = [tts_session_sup],
    tts_data:destroy(),
    tts_data:init(),
    ok = test_util:meck_new(MeckModules),
    NewSession = fun(ID) ->
                         tts_session:start_link(ID)
                 end,
    ok = meck:expect(tts_session_sup, new_session,NewSession),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    tts_data:destroy(),
    ok = test_util:meck_done(MeckModules),
    ok.
