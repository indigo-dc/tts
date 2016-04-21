-module(tts_user_cache_test).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = tts_user_cache:start_link(),
    ok = tts_user_cache:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100).

get_user_info_test() ->
    application:set_env(tts, cache_max_entries, 10),
    Issuer = <<"https://my.provider">>,
    Sub1 = <<"foo">>,
    Sub2 = <<"bar">>,
    Sub3 = <<"joe">>,
    LookupFun = fun(I, S) ->
                        I = Issuer,
                        case S of
                            Sub1 ->
                                {ok, #{uid => Sub1}};
                            _ ->
                                {error, not_found}
                        end
                end,
    InsertFun = fun(Info, MaxEntries) ->
                        MaxEntries = 10,
                        #{ uid := Sub2} = Info,
                        ok
                end,
    IdhFun = fun(UserInfo) ->
                        #{ type := openidconnect,
                           subject := Sub,
                           issuer := Issuer
                         } = UserInfo,
                        case Sub of
                            Sub2 -> {ok, #{uid => Sub2}};
                            Sub3 -> {error, not_allowed}
                        end
                end,
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, user_lookup_info, LookupFun),
    ok = meck:expect(tts_data, user_insert_info, InsertFun),

    ok = meck:new(tts_idh),
    ok = meck:expect(tts_idh, lookup_user, IdhFun),

    {ok, Pid} = tts_user_cache:start_link(),
    {ok, #{uid := Sub1}} = tts_user_cache:get_user_info(Issuer, Sub1),
    {ok, #{uid := Sub2}} = tts_user_cache:get_user_info(Issuer, Sub2),
    {error, not_allowed} = tts_user_cache:get_user_info(Issuer, Sub3),

    ok = tts_user_cache:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),

    true = meck:validate(tts_data),
    true = meck:validate(tts_idh),
    ok = meck:unload(tts_data),
    ok = meck:unload(tts_idh),
    ok.

verify_cache_test() ->
    CacheTimeout = 10000,
    application:set_env(tts, cache_check_interval, 10),
    application:set_env(tts, cache_timeout, CacheTimeout),

    MyPid = self(),

    DeleteFun = fun(T) ->
                        T = CacheTimeout,
                        MyPid ! cache_checked,
                        {ok, 0}
                end,
    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, user_delete_entries_older_than, DeleteFun),

    {ok, Pid} = tts_user_cache:start_link(),

    receive
        cache_checked -> ok
    end,

    ok = tts_user_cache:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),

    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    application:unset_env(tts, cache_check_interval),
    application:unset_env(tts, cache_timeout),
    ok.


clear_cache_test() ->
    {ok, Pid} = tts_user_cache:start_link(),

    ok = meck:new(tts_data),
    ok = meck:expect(tts_data, user_clear_cache, fun() -> ok end),

    ok = tts_user_cache:clear_cache(),

    ok = tts_user_cache:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),

    true = meck:validate(tts_data),
    ok = meck:unload(tts_data),
    ok.


garbage_test() ->
    {ok, Pid} = tts_user_cache:start_link(),

    ignored = gen_server:call(Pid, some_unsupported_call),
    ok = gen_server:cast(Pid, some_unsupported_cast),
    Pid ! some_unsupported_msg,

    ok = tts_user_cache:stop(),
    ok = test_util:wait_for_process_to_die(Pid,100),
    ok.
