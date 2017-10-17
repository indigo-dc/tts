-module(watts_persistent_leveldb).
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
-behaviour(watts_persistent).
-include("watts.hrl").

%% watts_persistent.
-export([initialize/0]).
-export([credential_add/5]).
-export([credential_get/1]).
-export([credential_get_list/1]).
-export([credential_get_count/2]).
-export([credential_remove/2]).
-export([is_ready/0]).



-record(credential, {
          id = undefined,
          uuid = undefined,
          ctime = undefined,
          interface = undefined,
          user_id = undefined,
          service_id = undefined,
          state = undefined
         }).

initialize() ->
    %% check if the leveldb directory exists
    Dir = ?CONFIG(eleveldb_dir),
    lager:info("Init: starting eleveldb database ~p", [Dir]),
    DirExists = filelib:is_dir(Dir),
    ok = stop_mnesia(),
    ok = setup_mnesia_if_needed(DirExists),
    ok = start_mnesia(),
    ok.

-spec credential_add(UserId::binary(), ServiceId::binary(),
                     Interface ::binary(), CredState :: any(),
                     AllowNonUniqueStates::boolean())
-> {ok, CredentialID ::binary()} | {error, Reason :: atom()}.
credential_add(UserId, ServiceId, Interface, CredState, SameStateAllowed) ->
    watts_persistent_mnesia:credential_add(UserId, ServiceId, Interface,
                                           CredState, SameStateAllowed).

-spec credential_get_list(UserId::binary()) -> {ok, [watts:cred()]}.
credential_get_list(UserId) ->
    watts_persistent_mnesia:credential_get_list(UserId).

-spec credential_get_count(UserId::binary(), ServiceId::binary()) ->
                                  {ok, pos_integer()}.
credential_get_count(UserId, ServiceId) ->
    watts_persistent_mnesia:credential_get_count(UserId, ServiceId).

-spec credential_get(CredId::binary()) -> {ok, watts:cred()} |
                                          {error, not_found}.
credential_get(Uuid) ->
    watts_persistent_mnesia:credential_get(Uuid).

-spec credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.
credential_remove(UserId, CredId) ->
    watts_persistent_mnesia:credential_remove(UserId, CredId).

-spec is_ready() -> true | {false, not_running}.
is_ready() ->
    case mnesia:table_info(credential, type) of
        set -> true;
        _ ->
            {false, not_running}
    end.



setup_mnesia_if_needed(true) ->
    ok;
setup_mnesia_if_needed(false) ->
    lager:info("Init: creating eleveldb schema and tables."),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    {ok, _} = mnesia_eleveldb:register(),
    ok = create_tables(),
    stop_mnesia(),
    ok.

create_tables() ->
    create_table(credential, record_info(fields, credential),
                 [uuid, user_id, service_id, state] ).

create_table(TableName, Attributes, Indexes) ->
    {ok, TableList} = list_tables(),
    Nodes = [ node() | nodes() ],
    case lists:member(TableName, TableList) of
        true ->
            ok;
        _->
            {atomic, ok} = mnesia:create_table(TableName,
                                               [{leveldb_copies, Nodes},
                                                {attributes, Attributes},
                                                {index, Indexes},
                                                {type, set}
                                               ]),
            mnesia:wait_for_tables([TableName], 10000)
    end.

list_tables() ->
    {ok, mnesia:system_info(local_tables)}.

stop_mnesia() ->
    application:stop(mnesia),
    application:set_env(mnesia, dir, ?CONFIG(eleveldb_dir)),
    ok.

start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    {ok, _} = mnesia_eleveldb:register(),
    ok.
