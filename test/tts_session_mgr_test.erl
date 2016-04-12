-module(tts_session_mgr_test).
-include_lib("eunit/include/eunit.hrl").


start_stop_test() ->
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
    ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
    {ok, Pid} = tts_session_mgr:start_link(),
    ok = tts_session_mgr:stop(),
    ok = wait_for_process_to_die(Pid, 100),
    ok = meck:unload(tts_data).


session_start_test() ->
    Session = self(),
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
    ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
    ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),

    ok = meck:new(tts_session_sup),
    ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),

    {ok, Pid} = tts_session_mgr:start_link(),
    {ok, Session} = tts_session_mgr:new_session(),

    ok = tts_session_mgr:stop(),
    ok = wait_for_process_to_die(Pid, 100),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    true = meck:validate(tts_session_sup),
    ok = meck:unload(tts_session_sup).


session_timeout_test() ->
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
    ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
    ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),


    ID = <<"some Id">>,
    {ok, Pid} = tts_session_mgr:start_link(),
    {ok, Session} = tts_session:start_link(ID),

    SessionList = [#{id => ID, pid => Session}],
    LookupFun = fun(Id) ->
                        case Id of
                            ID -> {ok, Session};
                            _ -> {error, not_found}
                        end
                end,
    ok = meck:expect(tts_data, sessions_get_pid , LookupFun),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),

    tts_session:set_max_age(1,Session),
    ok = wait_for_process_to_die(Session, 100),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
    ok = tts_session_mgr:stop(),
    ok = wait_for_process_to_die(Pid, 100),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data).


session_lookup_create_test() ->
    ok = meck:new(tts_session_sup),
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),

    ID = <<"some other Id">>,
    {ok, Pid} = tts_session_mgr:start_link(),

    Session = self(),
    SessionList = [#{id => ID, pid => Session}],
    LookupFun = fun(Id) ->
                        case Id of
                            ID -> {ok, Session};
                            _ -> {error, not_found}
                        end
                end,
    %% create a new session
    ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
    ok = meck:expect(tts_data, sessions_get_pid
                     , fun(_ID) -> {error, not_found} end),
    ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
    {ok, Session} = tts_session_mgr:get_session(undefined),

    %% Lookup an existing session
    ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> not_used end),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),
    ok = meck:expect(tts_data, sessions_get_pid , LookupFun),
    ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> {error,false} end),
    {ok, Session} = tts_session_mgr:get_session(ID),

    %% try to lookup a session, yet create it as it does not exist
    ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),
    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
    ok = meck:expect(tts_data, sessions_get_pid
                     , fun(_ID) -> {error, not_found} end),
    ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
    {ok, Session} = tts_session_mgr:get_session(ID),

    ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
    ok = tts_session_mgr:stop(),
    ok = wait_for_process_to_die(Pid, 100),

    true = meck:validate(tts_session_sup),
    ok = meck:unload(tts_session_sup),
    true = meck:validate(tts_data),
    ok = meck:unload(tts_data).


session_deletion_test() ->
    ok = meck:new(tts_data),
    ID = <<"ID123">>,
    {ok, Session} = tts_session:start_link(ID),
    SessionList = [#{id => ID, pid => Session}],
    ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),
    ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),

    {ok, Pid} = tts_session_mgr:start_link(),
    ok = tts_session_mgr:close_all_sessions(),
    ok = wait_for_process_to_die(Session, 100),

    ok = tts_session_mgr:stop(),
    ok = wait_for_process_to_die(Pid, 100),

    true = meck:validate(tts_data),
    ok = meck:unload(tts_data).



wait_for_process_to_die(_Pid,0) ->
    still_alive;
wait_for_process_to_die(Pid,Iterations) ->
    case process_info(Pid) of
        undefined ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_process_to_die(Pid,Iterations-1)
    end.
