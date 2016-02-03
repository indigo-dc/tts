-module(tts_app).
-behaviour(application).

-include("tts.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    tts_data:init(),
    tts_config:read_configs(),

    EpMain = ?CONFIG(ep_main),
    EpRedirect = ?CONFIG(ep_redirect),
    EpReturn = ?CONFIG(ep_return),
    Dispatch = [{'_', [
                       {"/api",tts_rest, []},
                       {EpMain, tts_http, []},
                       {EpRedirect, tts_http, []},
                       {EpReturn, tts_http, []}
                      ]}],
    {ok, _} = cowboy:start_http( http_handler 
                       , 100
                       , [ {port, 8080} ]
                       , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
                     ),
	tts_sup:start_link().

stop(_State) ->
	ok.
