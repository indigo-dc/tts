-module(tts_http_util_test).
-include_lib("eunit/include/eunit.hrl").

perform_cookie_action_test() ->
    {ok, Meck} = start_meck(),
    {ok, req2} = tts_http_util:perform_cookie_action(clear, ignored, ignored, req),
    {ok, req2} = tts_http_util:perform_cookie_action(update, 0, ignored, req),
    application:set_env(tts,ssl, true),
    {ok, req2} = tts_http_util:perform_cookie_action(update, ignored, deleted, req),
    {ok, req2} = tts_http_util:perform_cookie_action(update, 10, <<"content">>, req),
    application:unset_env(tts,ssl),
    ok = stop_meck(Meck),
    ok.


relative_path_test() ->
    application:set_env(tts,ssl, true),
    %% the config ensures that ep_main always ends on /
    application:set_env(tts, ep_main, <<"/non_default/">>),
    <<"/non_default/sub/">> = tts_http_util:relative_path("sub/"),
    ok.

whole_url_test() ->
    Path = "/api",
    application:set_env(tts, hostname, "localhost"),
    TestUrl =
        fun({Ssl, Port, Exp}, Other) ->
                application:set_env(tts, ssl, Ssl),
                application:set_env(tts, port, Port),
                Exp = tts_http_util:whole_url(Path),
                application:unset_env(tts, ssl),
                application:unset_env(tts, port),
                Other
        end,
    Tests =
        [
         {false, 8080, <<"http://localhost:8080/api">>},
         {false, 443, <<"http://localhost:443/api">>},
         {false, 80, <<"http://localhost/api">>},
         {true, 8443, <<"https://localhost:8443/api">>},
         {true, 80, <<"https://localhost:80/api">>},
         {true, 443, <<"https://localhost/api">>}
        ],
   lists:foldl(TestUrl, ignored, Tests),
   ok.




start_meck() ->
    MeckModules = [cowboy_req],
    SetCookie = fun(Name, _Value, _Opts, Req) ->
                        Name = tts_http_util:cookie_name(),
                        case Req of
                            req ->
                                req2;
                            _ ->
                                {error, no_request}
                        end
                end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, set_resp_cookie, SetCookie),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
