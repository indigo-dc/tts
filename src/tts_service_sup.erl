-module(tts_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [service_worker()],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.

service_worker() ->
    #{id => service_worker,
      start => {tts_service_worker,start_link,[]},
      restart => transient
     }.
