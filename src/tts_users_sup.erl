-module(tts_users_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    UserSup = #{ 
      id => user_sup,
      start => {tts_user_sup, start_link, []},
      type => supervisor
     },
    UserMgr = #{ 
      id => user_mgr,
      start => {tts_user_mgr, start_link, []}
     },
	Procs = [UserSup, UserMgr],
    Flags = #{ strategy => one_for_all },
	{ok, {Flags, Procs}}.
