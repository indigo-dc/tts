-module(tts_session_sup).
%%
%% Copyright 2016 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").

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
