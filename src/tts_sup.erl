-module(tts_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SessionsSup = #{ 
      id => sessions_sup, 
      start => {tts_sessions_sup, start_link, []},
      type => supervisor
     },
	Procs = [SessionsSup],
    Flags = #{},
	{ok, {Flags,  Procs}}.
