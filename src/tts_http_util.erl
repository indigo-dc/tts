-module(tts_http_util).
-include("tts.hrl").
-export([
         perform_cookie_action/4,
         create_cookie_opts/1,
         cookie_name/0
        ]).

-define(COOKIE, <<"tts_session">>).

perform_cookie_action(clear, _MaxAge, _Value, Req) ->
    Opts = create_cookie_opts(0),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, Opts, Req),
    {ok, Req2};
perform_cookie_action(update, 0, _, Req) ->
    perform_cookie_action(clear, 0, deleted, Req);
perform_cookie_action(update, _, deleted, Req) ->
    perform_cookie_action(clear, 0, deleted, Req);
perform_cookie_action(update, MaxAge, Value, Req) ->
    Opts = create_cookie_opts(MaxAge),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, Value, Opts, Req),
    {ok, Req2}.

create_cookie_opts(MaxAge) ->
    BasicOpts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    case ?CONFIG(ssl) of
        true ->
            [{secure, true} | BasicOpts];
        _ ->
            BasicOpts
    end.

cookie_name() ->
    ?COOKIE.
