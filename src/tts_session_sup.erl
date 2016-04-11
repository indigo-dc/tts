-module(tts_session_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([new_session/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_session(ID) ->
    supervisor:start_child(?MODULE, [ID]).

init([]) ->
    Session = #{
      id => session,
      start => {tts_session, start_link, []},
      restart => transient
     },
    Procs = [Session],
    Flags = #{ strategy => simple_one_for_one  },
    {ok, {Flags, Procs}}.
