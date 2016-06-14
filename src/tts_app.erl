-module(tts_app).
-behaviour(application).

-include("tts.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    lager:info("Token Translation Service starting"),
    tts_data:init(),
    tts_sup:start_link().

stop(_State) ->
    ok.
