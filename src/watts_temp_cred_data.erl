-module(tts_temp_cred_data).
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

-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([start/1]).
-export([get_credential/1]).
-export([stop/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("tts.hrl").

-record(state, {
          credential = undefined
         }).

%% API.

-spec start_link(Credential :: list()) -> {ok, pid()}.
start_link(Credential) ->
    gen_server:start_link(?MODULE, Credential, []).

-spec start(Credential :: list()) -> {ok, pid()}.
start(Credential) ->
    gen_server:start(?MODULE, Credential, []).

-spec get_credential(Pid::pid()) -> {ok, list()}.
get_credential(Pid) ->
    gen_server:call(Pid, get_credential, 1000).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

init(Credential) ->
    State = #state{
               credential = Credential
              },
    Timeout = ?CONFIG(credential_timeout, 10000),
    {ok, State, Timeout}.

handle_call(get_credential, _From, #state{credential=Credential}=State) ->
    {stop, normal, {ok, Credential} , State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
