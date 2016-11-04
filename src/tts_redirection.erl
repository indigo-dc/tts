-module(tts_redirection).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).


init(_Type, Req, _Opts) ->
    {ok, Req, []}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, _State) ->
    Url = tts_http_util:whole_url(""),
    redirect_to(Url, Req).


redirect_to(Url, Req) ->
    Header = [{<<"location">>, Url}],
    {ok, Req2} = cowboy_req:reply(302, Header, Req),
    {ok, Req2, []}.
