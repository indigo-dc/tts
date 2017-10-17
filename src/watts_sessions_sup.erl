%% @doc the supervisor watching the session manager and the session supervisor
-module(watts_sessions_sup).
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

%% @doc start the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

%% @doc intialize the supervisor
init(noparams) ->
    SessionSup = #{
      id => session_sup,
      start => {watts_session_sup, start_link, []},
      type => supervisor
     },
    SessionMgr = #{
      id => session_mgr,
      start => {watts_session_mgr, start_link, []}
     },
    Procs = [SessionSup, SessionMgr],
    Flags = #{ strategy => one_for_all },
    {ok, {Flags, Procs}}.
