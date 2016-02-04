-module(tts_user_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([add_user/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add_user(UserSubject) ->
    supervisor:start_child(?MODULE,[UserSubject]).

init([]) ->
    User = #{ 
      id => user, 
      start => {tts_user,start_link,[]}, 
      restart => transient
     },
	Procs = [User],
    Flags = #{ strategy => simple_one_for_one  },
	{ok, {Flags, Procs}}.
