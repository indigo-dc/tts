-module(watts_session_mgr_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

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

        {ok, _, JWT} = watts_session_mgr:get_cookie_data(Session),
        {ok, Session} = watts_session_mgr:get_session(JWT),
        watts_session:set_max_age(1,Session),
        ok = test_util:wait_for_process_to_die(Session, 100),

        {ok, undefined} = watts_session_mgr:get_session(JWT),
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
        {ok, _, JWT} = watts_session_mgr:get_cookie_data(Session),

        %% Lookup an existing session
        {ok, Session} = watts_session_mgr:get_session(JWT),
        ok = watts_session:close(Session),
        ok = test_util:wait_for_process_to_die(Session, 100),

        %% try to lookup the closed session, yet fail and return undefined
        {ok, undefined} = watts_session_mgr:get_session(JWT),

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
    ok = meck:expect(watts_session_sup, new_session, NewSession),
    Key = #{d =>
      <<"vB-EV7HFTOXiOsyBOEc_c1E-Z4ydcYUoqZqO46F35xJZdLNKAnesn3O1t52s8mj5UZuae4RJlX-32eQ7rWEfjG-GsJ_G9uB0VxR4AjzC34cNYiy-DeprqJ704qaO2ptaxkhmNg-WBHoXbJamR10xrXuxjankmlJKkJ3bFS87c-UiW0pcqbhKPz3YRP3uLVoP7HXPTFhW5I6je3jGOaNcaUnuri3dfV1STXbIQ5nGdm_88HqyxJ1xpPYolkxOqUySUziw1tyd5zB94tJ6_xNRwRFcFMts8U9dt0JNNC_MK3cfpvzHtmCTmKimNvicKvhV835Iyrt4NNWNOKQHbbl64Q">>,
  e => <<"AQAB">>,kty => <<"RSA">>,
  n =>
      <<"xw3lyt9tLzjghDCfZQZW0tgKFpReQvOVsJtgQQIF-HQxmBQqHa7e1xMkMgq3Kmwn0kIIJZ-NkXwKuEX9PubdrSaoc7BxcQon9R_2TneaE8wsgtCxVx7uTD4vgUZcZ-Z-7ZIotq1DOvbVDQtpPzobvIC4JXotbsGI9deouFlHbQA6mw9Cvh32ylB43C6NFJCFEpfB2-fBuat5gOSm02CIs1qASd9C0g3WPf7HvR9f3Ot3G7JaH6PA5u7AnKcQ1UtGI1zfX5m6qBj7mhaRFtEood8Jo_lUqYQP4pr_lk3UQLhpWETdMv_QVGsRFqSwuGLbOmRQOoMuC1eifuXcKeXtUw">>},
    ?SETCONFIG(jwt_key, Key),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ?UNSETCONFIG(jwt_key),
    watts_ets:destroy(),
    ok = test_util:meck_done(MeckModules),
    ok.
