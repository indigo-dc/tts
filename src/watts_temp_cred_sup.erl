%% @doc supervisor module for the temporary credential store
-module(watts_temp_cred_sup).
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
-export([new_temp_cred/1]).

%% @doc start_link implementation, starting the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

%% @doc start a new data process and supervise it
-spec new_temp_cred(Credential :: watts:credential()) -> {ok, pid()}.
new_temp_cred(Credential) ->
    supervisor:start_child(?MODULE, [Credential]).


%% spec failing ... skipping it
%% -spec init(noparams)
%%           -> {ok, {
%%                 #{ strategy => simple_one_for_one  }
%%                   ,[#{id => temp_cred,
%%                       start => {watts_temp_cred_data, start_link, []},
%%                       restart => transient}
%%                    ]}}.
%% @doc initialize the supervisor with the process type to start
init(noparams) ->
    TempCred = #{
      id => temp_cred,
      start => {watts_temp_cred_data, start_link, []},
      restart => transient
     },
    Procs = [TempCred],
    Flags = #{ strategy => simple_one_for_one  },
    {ok, {Flags, Procs}}.
