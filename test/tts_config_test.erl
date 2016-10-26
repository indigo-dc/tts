-module(tts_config_test).
-include_lib("eunit/include/eunit.hrl").


start_stop_test() ->
    {ok, Pid} = tts_config:start_link(),
    ok = tts_config:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.

macro_test() ->
    application:set_env(tts,conf_loaded,true),
    application:set_env(tts,debug_mode,true),
    true = tts_config:is_loaded(),
    true = tts_config:debug_mode(),
    application:set_env(tts,conf_loaded,false),
    application:set_env(tts,debug_mode,false),
    false = tts_config:is_loaded(),
    false = tts_config:debug_mode(),
    application:unset_env(tts,conf_loaded),
    application:unset_env(tts,debug_mode),
    false = tts_config:is_loaded(),
    false = tts_config:debug_mode(),
    ok.

autostop_test() ->
    MeckModules = [tts_session_mgr, tts_data_sqlite,
                   oidcc_client, cowboy_router, cowboy],

    OkFun = fun() -> ok end,
    CompileFun = fun(_) -> routing end,
    HttpStartFun = fun(_, _, _, _) -> ok end,

    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_session_mgr, close_all_sessions, OkFun),
    ok = meck:expect(tts_data_sqlite, reconfigure, OkFun),
    ok = meck:expect(cowboy_router, compile, CompileFun),
    ok = meck:expect(cowboy, start_http, HttpStartFun),
    ok = meck:expect(oidcc_client, register, fun(_) -> ok end),

    {ok, Pid} = tts_config:start_link(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok = test_util:meck_done(MeckModules),
    ok.

%% config_read_test() ->
%%     application:ensure_all_started(econfig),
%%     MeckModules = [tts_session_mgr, tts_service, tts_data_sqlite, cowboy_router,
%%                    cowboy, oidcc, oidcc_client],

%%     OkFun = fun() -> ok end,
%%     CompileFun = fun(_) -> routing end,
%%     HttpStartFun = fun(_, _, _, _) -> {ok, some_info} end,
%%     HttpStopFun = fun(_) -> ok end,
%%     AddOidcFun = fun(_, _, _, _, _, _, _, _) -> {ok, id, pid} end,
%%     AddServiceFun = fun(_) -> ok end,

%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_session_mgr, close_all_sessions, OkFun),
%%     ok = meck:expect(tts_data_sqlite, reconfigure, OkFun),
%%     ok = meck:expect(tts_service, add, AddServiceFun),
%%     ok = meck:expect(cowboy_router, compile, CompileFun),
%%     ok = meck:expect(cowboy, start_http, HttpStartFun),
%%     ok = meck:expect(cowboy, start_https, HttpStartFun),
%%     ok = meck:expect(cowboy, stop_listener, HttpStopFun),
%%     ok = meck:expect(oidcc, add_openid_provider, AddOidcFun),
%%     ok = meck:expect(oidcc_client, register, fun(_) -> ok end),

%%     ConfigPath = filename:absname("./test/config_test"),
%%     application:set_env(tts, config_path, ConfigPath),

%%     {ok, Pid} = tts_config:start_link(),
%%     ok = test_util:wait_for_process_to_die(Pid,100),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.
