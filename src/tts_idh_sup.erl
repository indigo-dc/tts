-module(tts_idh_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
             idh(),
             worker_supervisor()
            ],
	{ok, {{rest_for_one, 1, 5}, Procs}}.

worker_supervisor() ->
    #{ id => worker_supervisor,
       start => {tts_idh_worker_sup, start_link, []},
       type => supervisor
     }.

idh() ->
    #{ id => idh,
       start => {tts_idh, start_link, []}
     }.
