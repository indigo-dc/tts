-module(tts_session_mgr_test).
-include_lib("eunit/include/eunit.hrl").


%% start_stop_test() ->
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
%%     {ok, Pid} = tts_session_mgr:start_link(),
%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid, 100),
%%     ok = meck:unload(tts_data).


%% session_start_test() ->
%%     Session = self(),
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
%%     ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
%%     ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),

%%     ok = meck:new(tts_session_sup),
%%     ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),

%%     {ok, Pid} = tts_session_mgr:start_link(),
%%     {ok, Session} = tts_session_mgr:new_session(),

%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid, 100),
%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data),
%%     true = meck:validate(tts_session_sup),
%%     ok = meck:unload(tts_session_sup).


%% session_timeout_test() ->
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
%%     ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
%%     ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),


%%     ID = <<"some Id">>,
%%     {ok, Pid} = tts_session_mgr:start_link(),
%%     {ok, Session} = tts_session:start_link(ID),

%%     SessionList = [#{id => ID, pid => Session}],
%%     LookupFun = fun(Id) ->
%%                         case Id of
%%                             ID -> {ok, Session};
%%                             _ -> {error, not_found}
%%                         end
%%                 end,
%%     ok = meck:expect(tts_data, sessions_get_pid , LookupFun),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),

%%     tts_session:set_max_age(1,Session),
%%     ok = test_util:wait_for_process_to_die(Session, 100),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid, 100),
%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data).


%% session_lookup_create_test() ->
%%     ok = meck:new(tts_session_sup),
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, sessions_update_pid, fun(_ID, _Pid) -> ok end),

%%     ID = <<"some other Id">>,
%%     {ok, Pid} = tts_session_mgr:start_link(),

%%     Session = self(),
%%     SessionList = [#{id => ID, pid => Session}],
%%     LookupFun = fun(Id) ->
%%                         case Id of
%%                             ID -> {ok, Session};
%%                             _ -> {error, not_found}
%%                         end
%%                 end,
%%     %% create a new session
%%     ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = meck:expect(tts_data, sessions_get_pid
%%                      , fun(_ID) -> {error, not_found} end),
%%     ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
%%     {ok, Session} = tts_session_mgr:new_session(),

%%     %% Lookup an existing session
%%     ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> not_used end),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),
%%     ok = meck:expect(tts_data, sessions_get_pid , LookupFun),
%%     ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> {error,false} end),
%%     {ok, Session} = tts_session_mgr:get_session(ID),

%%     %% try to lookup a session, yet fail and return undefined
%%     ok = meck:expect(tts_session_sup, new_session, fun(_ID) -> {ok,Session} end),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = meck:expect(tts_data, sessions_get_pid
%%                      , fun(_ID) -> {error, not_found} end),
%%     ok = meck:expect(tts_data, sessions_create_new, fun(_ID) -> ok end),
%%     {ok, undefined} = tts_session_mgr:get_session(ID),

%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid, 100),

%%     true = meck:validate(tts_session_sup),
%%     ok = meck:unload(tts_session_sup),
%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data).


%% session_deletion_test() ->
%%     ok = meck:new(tts_data),
%%     ID = <<"ID123">>,
%%     {ok, Session} = tts_session:start_link(ID),
%%     SessionList = [#{id => ID, pid => Session}],
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> SessionList end),
%%     ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),

%%     {ok, Pid} = tts_session_mgr:start_link(),
%%     ok = tts_session_mgr:close_all_sessions(),
%%     ok = test_util:wait_for_process_to_die(Session, 100),

%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid, 100),

%%     true = meck:validate(tts_data),
%%     ok = meck:unload(tts_data).


%% garbage_test() ->
%%     ok = meck:new(tts_data),
%%     ok = meck:expect(tts_data, sessions_get_list, fun() -> [] end),
%%     ok = meck:expect(tts_data, sessions_delete, fun(_ID) -> true end),
%%     {ok, Pid} = tts_session_mgr:start_link(),
%%     Pid ! <<"some data">>,
%%     ok = gen_server:cast(Pid,some_cast),
%%     ignored = gen_server:call(Pid,some_unsupported_call),
%%     still_alive = test_util:wait_for_process_to_die(Pid,5),
%%     ok = tts_session_mgr:stop(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = meck:unload(tts_data).
