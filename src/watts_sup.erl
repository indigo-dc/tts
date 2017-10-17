%% @doc the main supervisor of the WaTTS application
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

%% @doc start the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

%% @doc intialize the supervisor
init(noparams) ->
    Procs = [
             sessions_supervisor(),
             rsp_keys_supervisor(),
             plugin_supervisor(),
             plugin_worker(),
             sqlite_worker(),
             temp_cred_sup(),
             temp_cred(),
             init_worker()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.


%% @doc the sessions supervisor configuration
-spec sessions_supervisor() -> map().
sessions_supervisor() ->
    #{ id => sessions_sup,
       start => {watts_sessions_sup, start_link, []},
       type => supervisor
     }.

%% @doc the rsp keys supervisor configuration
%% @todo: only start if RSP is enabled
-spec rsp_keys_supervisor() -> map().
rsp_keys_supervisor() ->
    #{ id => rsp_keys_sup,
       start => {watts_rsp_keys_sup, start_link, []},
       type => supervisor
     }.

%% @doc the plugin worker configuration
-spec plugin_worker() -> map().
plugin_worker() ->
    #{ id => plugin,
       start => {watts_plugin, start_link, []}
     }.

%% @doc the plugin supervisor configuration
-spec plugin_supervisor() -> map().
plugin_supervisor() ->
    #{ id => plugin_sup,
       start => {watts_plugin_sup, start_link, []},
       type => supervisor
     }.

%% @doc the init worker configuration
-spec init_worker() -> map().
init_worker() ->
    #{ id => init,
       start => {watts_init, start_link, []},
       restart => transient
     }.

%% @doc the sqlite worker configuration
%% @todo: only start sqlite worker if needed
-spec sqlite_worker() -> map().
sqlite_worker() ->
    #{ id => sqlite,
       start => {watts_persistent_sqlite, start_link, []}
     }.

%% @doc the temp_cred worker configuration
-spec temp_cred() -> map().
temp_cred() ->
    #{ id => temp_cred,
       start => {watts_temp_cred, start_link, []}
     }.

%% @doc the temp_cred supervisor configuration
-spec temp_cred_sup() -> map().
temp_cred_sup() ->
    #{ id => temp_cred_sup,
       start => {watts_temp_cred_sup, start_link, []},
       type => supervisor
     }.
