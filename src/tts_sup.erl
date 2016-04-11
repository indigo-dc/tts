-module(tts_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([restart_config/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

restart_config() ->
    supervisor:restart_child(?MODULE, config).

init([]) ->
    Procs = [
             sessions_supervisor(),
             credential_supervisor(),
             credential_worker(),
             idh_supervisor(),
             user_cache(),
             sqlite_worker(),
             config_worker()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.


sessions_supervisor() ->
    #{ id => sessions_sup,
       start => {tts_sessions_sup, start_link, []},
       type => supervisor
     }.

credential_worker() ->
    #{ id => credential,
       start => {tts_credential, start_link, []}
     }.

credential_supervisor() ->
    #{ id => credential_sup,
       start => {tts_cred_sup, start_link, []},
       type => supervisor
     }.

config_worker() ->
    #{ id => config,
       start => {tts_config, start_link, []},
       restart => transient
     }.

sqlite_worker() ->
    #{ id => sqlite,
       start => {tts_data_sqlite, start_link, []}
     }.
idh_supervisor() ->
    #{ id => idh_supervisor,
       start => {tts_idh_sup, start_link, []},
       type => supervisor
     }.

user_cache() ->
    #{ id => user_cache,
       start => {tts_user_cache, start_link, []}
     }.
