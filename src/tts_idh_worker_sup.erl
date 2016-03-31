-module(tts_idh_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_worker/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    IDHWorker = #{ 
      id => idh_worker, 
      start => {tts_idh_worker,start_link,[]}, 
      restart => transient
     },
	Procs = [IDHWorker],
    Flags = #{ strategy => simple_one_for_one  },
	{ok, {Flags, Procs}}.
