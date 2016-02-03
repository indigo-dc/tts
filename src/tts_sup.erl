-module(tts_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
             session_supervisor(),
             config_worker()
            ],
    Flags = #{},
	{ok, {Flags,  Procs}}.


session_supervisor() ->
    #{ id => sessions_sup, 
       start => {tts_sessions_sup, start_link, []},
       type => supervisor
     }.

config_worker() ->
    #{ id => config,
       start => {tts_config, start_link, []},
       restart => transient
     }.
