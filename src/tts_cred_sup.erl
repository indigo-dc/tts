-module(tts_cred_sup).
-behaviour(supervisor).

-export([new_worker/0]).

-export([start_link/0]).
-export([init/1]).


new_worker() ->
    supervisor:start_child(?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CredWorker = #{
      id => cred_worker,
      start => {tts_cred_worker, start_link, []},
      restart => transient
     },
    Procs = [CredWorker],
    Flags = #{ strategy => simple_one_for_one  },
    {ok, {Flags, Procs}}.
