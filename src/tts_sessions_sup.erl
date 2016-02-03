-module(tts_sessions_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SessionSup = #{ 
      id => session_sup,
      start => {tts_session_sup, start_link, []},
      type => supervisor
     },
    SessionMgr = #{ 
      id => session_mgr,
      start => {tts_session_mgr, start_link, []}
     },
	Procs = [SessionSup, SessionMgr],
    Flags = #{ strategy => one_for_all },
	{ok, {Flags, Procs}}.
