%% @doc This module defines the needed callbacks for WaTTS persistent storage.
%% Any module implementing these can be used to store credentials. So extending
%% the supported databases woule mean to implement a new watts_persistent_xxx
%% module.
%% Also the config/schema/watts.schema needs to be adjusted to supporte the new
%% database and its config.
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
-export([credential_fetch/2]).
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
    {ok, [watts:credential()]}.

-callback credential_get_count(UserId::binary(), ServiceId::binary()) ->
    {ok, pos_integer()}.

-callback credential_get(CredId::binary()) ->
    {ok, watts:credential()} | {error, Reason :: atom()}.

-callback credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.

-callback is_ready() -> true | {false, Reason :: any()}.

%% @doc this gets called during initialization.
%% The function calls the initalize function of the configured
%% database.
-spec init() -> ok | {error, Reason::atom()}.
init() ->
    Mod = mod(),
    Mod:initialize().

%% @doc storing a credential, this is done by calling the configured
%% database and credential_add there.
-spec credential_store(UserId :: binary(), ServiceId :: binary(),
                       Interface :: binary(), CredState :: any(),
                       AllowNonUniqueStates :: boolean()) ->
                              {ok, CredentialId :: binary()} |
                              {error, Reason :: atom()}.
credential_store(UserId, ServiceId, Interface, CredState,
                 AllowNonUniqueStates) ->
    Mod = mod(),
    Mod:credential_add(UserId, ServiceId, Interface, CredState,
                       AllowNonUniqueStates).


%% @doc fetch the list of all credentials for a user.
%% using the configured database.
-spec credential_fetch_list(UserId :: binary()) -> {ok, [watts:credential()]}.
credential_fetch_list(UserId) ->
    Mod = mod(),
    Mod:credential_get_list(UserId).

%% @doc get the number of credentials of the user at the service.
%% using the configured database.
-spec credential_service_count(UserId :: binary(), ServiceId :: binary())
                              -> {ok, pos_integer()}.
credential_service_count(UserId, ServiceId) ->
    Mod = mod(),
    Mod:credential_get_count(UserId, ServiceId).


%% @doc get the credential with the Id for the given User.
%% using the configured database.
-spec credential_fetch(CredId :: binary(), UserId :: binary())
                      -> {ok, watts:credential()} | {error, Reason :: atom()}.
credential_fetch(CredId, UserId) ->
    Mod = mod(),
    CredResult = Mod:credential_get(CredId),
    ensure_credential_of_user(CredResult, UserId).

%% @doc this function ensures that credentials are only returned to their owner.
-spec ensure_credential_of_user({ok, watts:credential()} |
                                {error, Reason:: atom()}, UserId :: binary())
                               -> {ok, watts:credential()} |
                                  {error, Reason :: atom()}.
ensure_credential_of_user({ok, #{user_id := UserId}} = Result, UserId) ->
    Result;
ensure_credential_of_user({ok, _}, _UserId) ->
    {error, bad_user};
ensure_credential_of_user({error, _} = Error, _UserId) ->
    Error.


%% @doc delete the credential with the Id.
%% using the configured database.
-spec credential_delete(UserId :: binary(), CredentialId :: binary())
                       -> ok | {error, Reason :: atom()}.
credential_delete(UserId, CredentialId) ->
    Mod = mod(),
    Mod:credential_remove(UserId, CredentialId).

%% @doc check if the configured database is ready.
-spec is_ready() -> true | {false, Reason :: any()}.
is_ready() ->
    Mod = mod(),
    Mod:is_ready().


%% @doc get the configured database module.
-spec mod() -> atom().
mod() ->
   {ok, Mod} = ?CONFIG_(?MOD),
    Mod.
