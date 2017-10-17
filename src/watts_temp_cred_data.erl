
%% @doc This module implements the temporary data storage holding the
%% translation result from a plugin.
%% It is implemented as a gen_server behaviour so that each result is
%% stored in its own process. The process dies once it has been read or
%% after a configurable timeout, 10 seconds by default.
-module(watts_temp_cred_data).
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

-include("watts.hrl").

-record(state, {
          credential = undefined :: undefined | watts:credential()
         }).

%% API.
-type state() :: #state{}.

%% @doc starting the linked gen_server with the credential
-spec start_link(Credential :: watts:credential()) -> {ok, pid()}.
start_link(Credential) ->
    gen_server:start_link(?MODULE, Credential, []).

%% @doc starting the non-linked gen_server with the credential.
-spec start(Credential :: watts:credential()) -> {ok, pid()}.
start(Credential) ->
    gen_server:start(?MODULE, Credential, []).

%% @doc sending a call to retrieve the credential
-spec get_credential(Pid::pid()) -> {ok, watts:credential()}.
get_credential(Pid) ->
    gen_server:call(Pid, get_credential, 1000).

%% @doc stopping the gen_server
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%% @doc initialize the process with a credential
-spec init(Credential :: watts:credential())
          -> {ok, state(), Timeout :: non_neg_integer()}.
init(Credential) ->
    State = #state{
               credential = Credential
              },
    Timeout = ?CONFIG(credential_timeout, 10000),
    {ok, State, Timeout}.

%% @doc implementation of 'handle_call'.
%% This just returns the credential if asked and then stops.
-spec handle_call(get_credential, any(), state())
                 -> {stop, normal, {ok, watts:credential()}, state()}.
handle_call(get_credential, _From, #state{credential=Credential}=State) ->
    {stop, normal, {ok, Credential} , State}.

%% @doc implementation of 'handle_cast'.
%% This only reacts to stop and then stops the process.
-spec handle_cast(stop, state()) -> {stop, normal, state()}.
handle_cast(stop, State) ->
    {stop, normal, State}.


%% @doc implementation of 'handle_info'.
%% This only reacts to timeout and then stops the process.
-spec handle_info(timeout, state()) -> {stop, normal, state()}.
handle_info(timeout, State) ->
    {stop, normal, State}.


%% @doc implementation of 'terminate' for the behaviour, does nothing.
-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc implementation of 'code_change' for the behaviour, does nothing.
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
