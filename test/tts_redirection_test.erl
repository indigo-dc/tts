-module(tts_redirection_test).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, req, []} = tts_redirection:init(ignored, req, ignored),
    ok = tts_redirection:terminate( ignored, ignored, ignored).


redirect_test() ->
    MeckModules = [cowboy_req],
    Reply = fun(Status, Header, Req) ->
                    LocationFound =
                        case lists:keyfind(<<"location">>,1,Header) of
                            {<<"location">>, _} ->
                                true;
                            _ ->
                                false
                        end,
                    case {Status, Req, LocationFound} of
                        {302, req, true} ->
                            {ok, req2};
                        _ ->
                            {error, not_a_redirection}
                    end
            end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, reply, Reply),
    set_needed_env(),
    {ok, req2, []} = tts_redirection:handle(req, ignored),
    unset_env(),
    ok = test_util:meck_done(MeckModules),
    ok.


set_needed_env() ->
    application:set_env(tts, hostname, "localhost"),
    application:set_env(tts, port, 8080),
    application:set_env(tts, ssl, false),
    ok.

unset_env() ->
    application:unset_env(tts, hostname),
    application:unset_env(tts, port),
    application:unset_env(tts, ssl),
    ok.
