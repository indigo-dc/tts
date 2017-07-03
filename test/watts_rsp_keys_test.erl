-module(watts_rsp_keys_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").


-define(URL1, <<"http://watts.test.one">>).
-define(URL2, <<"https://watts.test.two">>).
-define(URL3, <<"https://watts.test.three">>).
-define(URL4, <<"https://watts.test.four">>).
-define(URL5, <<"https://watts.test.five">>).
-define(URL6, <<"https://watts.test.six">>).
-define(KEYS1, <<"{\"keys\": [{\"kty\": \"RSA\", \"alg\": \"RS256\", \"use\": \"sig\", \"kid\": \"Jg3hQwpuoUCtOv19ed8CodLI4nTBqnEN\", \"n\": \"teK/NIbm7wyvPntcftbbzEYBq4Wf3pvpLZUE5kndrzU7uYXnIDvOsrTt2tM4oqKNj8BMSY+s7ebGaaXCesc02KBLNnl5cA/PGISjzE/LZB4TphQdTcA7AO3UKwgJXtCCt7Tmitv7MKaZTpVDI7lQZanKEkSi7F8YlLvC6GtgOZ2ip+B3vw4v60ULvS7FQu/QGib6H/eYa1l+r0lHxPQvqxaj40IsIOZmQrADshFRRO1MF6Lt9BTBjelAmGLNbBVTEVAU/0PythxTEgqtSrhdeW9agN8zUJuePuRPpDGajc9bJanvdPlRKKchaW2CerJdgF/y/cJBY2mWuPgIjciQZ5sYHHZGYVbfN9ViySeEh4jOcOVGW55YR4dg1R79KYv/MXfwNkELfgq5yIu34DbhKhFTr1TyaXDBHMr2+6mJ+hPrAJKHa1fBM1VxOITeZM6o9Jah7z5zoYLphR2hDuSVYxwdsKOHtRY+2/MDEKuTQm+M0W89/nEvq19VWkFsHIcuM3ljgdO/c5F7ImN3xxCB2IhDcG+GxEBTrFAQOxm0P28MjZ8Bw5WO7Rm2rbYs6z9MdUBD9bvJ5Xs5FTPYECFKoV93k1WRGVACiZ9tLy/rx8bsGnala913DMb3302dvxzQbBeTAgotF3JBXrOgOOyw513y872ZvUIfwxGcqrHs1AE=\", \"e\": \"AQAB\"}]}">>).
-define(KEYS2, "{\"keys\": [{\"kty\": \"RSA\", \"alg\": \"RS256\", \"use\": \"sig\", \"kid\": \"Jg3hQwpuoUCtOv19ed8CodLI4nTBqnEN\", \"n\": \"teK/NIbm7wyvPntcftbbzEYBq4Wf3pvpLZUE5kndrzU7uYXnIDvOsrTt2tM4oqKNj8BMSY+s7ebGaaXCesc02KBLNnl5cA/PGISjzE/LZB4TphQdTcA7AO3UKwgJXtCCt7Tmitv7MKaZTpVDI7lQZanKEkSi7F8YlLvC6GtgOZ2ip+B3vw4v60ULvS7FQu/QGib6H/eYa1l+r0lHxPQvqxaj40IsIOZmQrADshFRRO1MF6Lt9BTBjelAmGLNbBVTEVAU/0PythxTEgqtSrhdeW9agN8zUJuePuRPpDGajc9bJanvdPlRKKchaW2CerJdgF/y/cJBY2mWuPgIjciQZ5sYHHZGYVbfN9ViySeEh4jOcOVGW55YR4dg1R79KYv/MXfwNkELfgq5yIu34DbhKhFTr1TyaXDBHMr2+6mJ+hPrAJKHa1fBM1VxOITeZM6o9Jah7z5zoYLphR2hDuSVYxwdsKOHtRY+2/MDEKuTQm+M0W89/nEvq19VWkFsHIcuM3ljgdO/c5F7ImN3xxCB2IhDcG+GxEBTrFAQOxm0P28MjZ8Bw5WO7Rm2rbYs6z9MdUBD9bvJ5Xs5FTPYECFKoV93k1WRGVACiZ9tLy/rx8bsGnala913DMb3302dvxzQbBeTAgotF3JBXrOgOOyw513y872ZvUIfwxGcqrHs1AE=\", \"e\": \"AQAB\"}]}").




start_stop_test() ->
    Config = #{id => <<"test">>, key_location => <<"file:///doesnotexist">>},
    {ok, Pid} =  watts_rsp_keys:start_link(Config),
    ok = watts_rsp_keys:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok.


http_key_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL1},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HttpPid ! {start, Pid},
        HasKeys = fun() ->
                          {ok, Keys} = watts_rsp_keys:get_keys(Pid),
                          Keys /= []
                  end,
        ok = test_util:wait_for_true(HasKeys, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

https_key_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL2},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HttpPid ! {start, Pid},
        HasKeys = fun() ->
                          {ok, Keys} = watts_rsp_keys:get_keys(Pid),
                          Keys /= []
                  end,
        ok = test_util:wait_for_true(HasKeys, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

refetch_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL2},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HasKeys = fun() ->
                          {ok, Keys} = watts_rsp_keys:get_keys(Pid),
                          Keys /= []
                  end,
        failed = test_util:wait_for_true(HasKeys, 100),
        HttpPid ! {start, Pid},
        ok = watts_rsp_keys:reread(Pid),
        ok = test_util:wait_for_true(HasKeys, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

fetch_fail_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL3},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HttpPid ! {start, Pid},
        HasError = fun() ->
                           {ok, Error} = watts_rsp_keys:get_last_error(Pid),
                           io:format("Error: ~p~n", [Error]),
                           {http_result, 404, <<>>} == Error
                  end,
        ok = test_util:wait_for_true(HasError, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.


bad_keys_fail_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL4},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HttpPid ! {start, Pid},
        HasError = fun() ->
                           {ok, Error} = watts_rsp_keys:get_last_error(Pid),
                           io:format("Error: ~p~n", [Error]),
                           case Error of
                               {key_page, _} -> true;
                               _ -> false
                           end
                  end,
        ok = test_util:wait_for_true(HasError, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

client_crash_fail_test() ->
    {ok, {_, HttpPid} = Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL5},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HttpPid ! {start, Pid},
        HasError = fun() ->
                           {ok, Error} = watts_rsp_keys:get_last_error(Pid),
                           io:format("Error: ~p~n", [Error]),
                           case Error of
                               {client_crash, boom} -> true;
                               _ -> false
                           end
                  end,
        ok = test_util:wait_for_true(HasError, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

client_init_fail_test() ->
    {ok, Meck} = start_meck(),
    try
        Config = #{id => <<"test">>, key_location => ?URL6},
        {ok, Pid} =  watts_rsp_keys:start_link(Config),
        HasError = fun() ->
                           {ok, Error} = watts_rsp_keys:get_last_error(Pid),
                           io:format("Error: ~p~n", [Error]),
                           case Error of
                               {http_request, _} -> true;
                               _ -> false
                           end
                  end,
        ok = test_util:wait_for_true(HasError, 100),
        ok = watts_rsp_keys:stop(Pid)
    after
        ok = stop_meck(Meck)
    end,
    ok.

garbage_test() ->
    Config = #{id => <<"test">>, key_location => <<"file:///doesnotexist">>},
    {ok, Pid} =  watts_rsp_keys:start_link(Config),
    Pid ! garbage,
    ok = gen_server:cast(Pid, garbage),
    ignored = gen_server:call(Pid, garbage),
    ok = watts_rsp_keys:stop(Pid),
    ok = test_util:wait_for_process_to_die(Pid, 100),
    ok.


start_meck() ->
    MeckModules = [oidcc_http_util],
    ok = test_util:meck_new(MeckModules),
    AsyncHttp =
        fun(_, Url, _) ->
                case Url of
                    ?URL1 -> {ok, id1};
                    ?URL2 -> {ok, id2};
                    ?URL3 -> {ok, id3};
                    ?URL4 -> {ok, id4};
                    ?URL5 -> {ok, id5};
                    ?URL6 -> {error, doh};
                    _ -> {error, not_implemented}
                end
        end,


    HttpData =
        fun() ->
                Pid = receive
                          {start, P} -> P
                      end,
                Pid ! {http, {id1, {{http, 200, <<"ok">>}, [], ?KEYS1}}},
                Pid ! {http, {id2, {{http, 200, <<"ok">>}, [], ?KEYS2}}},
                Pid ! {http, {id3, {{http, 404, <<"not found">>}, [], <<>>}}},
                Pid ! {http, {id4, {{http, 200, <<"ok">>}, [], <<"{\"kez\":false}">>}}},
                Pid ! {http, {id5, {error, boom}}},
                ok
        end,

    HttpPid = spawn(HttpData),

    FakeUncompress = fun(Body, _) -> {ok, Body} end,
    ok = meck:expect(oidcc_http_util, async_http, AsyncHttp),
    ok = meck:expect(oidcc_http_util, uncompress_body_if_needed, FakeUncompress),
    {ok, {MeckModules, HttpPid}}.


stop_meck({MeckModules, HttpPid}) ->
    HttpPid ! {start, self()},
    test_util:wait_for_process_to_die(HttpPid, 100),
    ok = test_util:meck_done(MeckModules).
