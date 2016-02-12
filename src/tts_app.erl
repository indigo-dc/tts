-module(tts_app).
-behaviour(application).

-include("tts.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    redbug:start(["tts_session","tts_session_mgr","tts_user","tts_user_mgr","tts_idh"],[{time,3600000},{msgs,10000}]),
    tts_data:init(),
   	tts_sup:start_link().

stop(_State) ->
	ok.
