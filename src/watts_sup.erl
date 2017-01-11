-module(watts_sup).
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

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
             sessions_supervisor(),
             plugin_supervisor(),
             plugin_worker(),
             sqlite_worker(),
             temp_cred_sup(),
             temp_cred(),
             init_worker()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.


sessions_supervisor() ->
    #{ id => sessions_sup,
       start => {watts_sessions_sup, start_link, []},
       type => supervisor
     }.

plugin_worker() ->
    #{ id => plugin,
       start => {watts_plugin, start_link, []}
     }.

plugin_supervisor() ->
    #{ id => plugin_sup,
       start => {watts_plugin_sup, start_link, []},
       type => supervisor
     }.

init_worker() ->
    #{ id => init,
       start => {watts_init, start_link, []},
       restart => transient
     }.

sqlite_worker() ->
    #{ id => sqlite,
       start => {watts_data_sqlite, start_link, []}
     }.

temp_cred() ->
    #{ id => temp_cred,
       start => {watts_temp_cred, start_link, []}
     }.

temp_cred_sup() ->
    #{ id => temp_cred_sup,
       start => {watts_temp_cred_sup, start_link, []},
       type => supervisor
     }.
