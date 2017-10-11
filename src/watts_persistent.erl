-module(watts_persistent).
%%
%% Copyright 2017 SCC/KIT
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
-include("watts.hrl").


-export([init/0]).
-export([credential_store/5]).
-export([credential_fetch_list/1]).
-export([credential_service_count/2]).
-export([credential_fetch/1]).
-export([credential_delete/2]).
-export([is_ready/0]).

-define(MOD, persistent_module).

%% API.
-callback initialize() -> ok | {error, Reason :: atom()}.
-callback credential_add(UserId::binary(), ServiceId::binary(),
                     Interface ::binary(), CredState :: any(),
                     AllowNonUniqueStates::boolean()) ->
    {ok, CredentialID ::binary()} | {error, Reason :: atom()}.

-callback credential_get_list(UserId::binary()) ->
    {ok, [watts:cred()]}.

-callback credential_get_count(UserId::binary(), ServiceId::binary()) ->
    {ok, integer()}.

-callback credential_get(CredId::binary()) ->
    {ok, watts:cred()} | {error, Reason :: atom()}.

-callback credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.

-callback is_ready() ->
    ok | {error, Reason::atom()}.


init() ->
    Mod = mod(),
    Mod:initialize().

credential_store(UserId, ServiceId, Interface, CredState,
                 AllowNonUniqueStates) ->
    Mod = mod(),
    Mod:credential_add(UserId, ServiceId, Interface, CredState,
                       AllowNonUniqueStates).


credential_fetch_list(UserId) ->
    Mod = mod(),
    Mod:credential_get_list(UserId).

credential_service_count(UserId, ServiceId) ->
    Mod = mod(),
    Mod:credential_get_count(UserId, ServiceId).

credential_fetch(CredId) ->
    Mod = mod(),
    Mod:credential_get(CredId).

credential_delete(UserId, CredentialId) ->
    Mod = mod(),
    Mod:credential_remove(UserId, CredentialId).

is_ready() ->
    Mod = mod(),
    Mod:is_ready().



mod() ->
   {ok, Mod} = ?CONFIG_(?MOD),
    Mod.
