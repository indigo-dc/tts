-module(tts_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
             sessions_supervisor(),
             service_supervisor(),
             idh(),
             user_cache(),
             config_worker()
            ],
    Flags = #{},
	{ok, {Flags,  Procs}}.


sessions_supervisor() ->
    #{ id => sessions_sup, 
       start => {tts_sessions_sup, start_link, []},
       type => supervisor
     }.

service_supervisor() ->
    #{ id => service_sup, 
       start => {tts_service_sup, start_link, []},
       type => supervisor
     }.

config_worker() ->
    #{ id => config,
       start => {tts_config, start_link, []},
       restart => transient
     }.

idh() ->
    #{ id => idh,
       start => {tts_idh, start_link, []}
     }.

user_cache() ->
    #{ id => user_cache,
       start => {tts_user_cache, start_link, []}
     }.
