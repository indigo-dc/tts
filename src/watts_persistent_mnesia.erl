%% @doc this module implement the persistent storage for the internal
%% Erlang database called mnesia.
%% As there is an extension for mnesia that uses leveldb, this also enables
%% support for the leveldb.
%% @see watts_persistent_leveldb
-module(watts_persistent_mnesia).
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

%% @doc initialize the sqlite database.
%% A callback from the watts_persistent behaviour.
-spec initialize() -> ok.
initialize() ->
    %% check if the mnesia directory exists
    Dir = ?CONFIG(mnesia_dir),
    lager:info("Init: starting mnesia database ~p", [Dir]),
    DirExists = filelib:is_dir(Dir),
    ok = stop_mnesia(),
    ok = setup_mnesia_if_needed(DirExists),
    ok = start_mnesia(),
    ok.


%% @doc store a credential for a user
%% A callback from the watts_persistent behaviour.
-spec credential_add(UserId::binary(), ServiceId::binary(),
                     Interface ::binary(), CredState :: any(),
                     AllowNonUniqueStates::boolean())
-> {ok, CredentialID ::binary()} | {error, Reason :: atom()}.
credential_add(UserId, ServiceId, Interface, CredState, SameStateAllowed) ->
    Id = {UserId, ServiceId, CredState},
    CTime = cowboy_clock:rfc1123(),
    ErrorOrId =
        fun(false, CId) ->
                case SameStateAllowed of
                    true ->
                        {ok, CId};
                    false ->
                        {error, not_unique_state}
                end;
           (true, _) ->
                NewUuid = create_random_uuid(),
                Entry = #credential{id = Id,
                                    uuid=NewUuid,
                                    ctime = CTime,
                                    interface=Interface,
                                    user_id = UserId,
                                    service_id = ServiceId,
                                    state = CredState},
                ok = mnesia:write(Entry),
                {ok, NewUuid}
        end,
    AddCredential = fun() ->
                            {Unique, CId}  =
                                case mnesia:read(credential, Id, write) of
                                    [] ->  {true, undefined};
                                    [#credential{uuid = Cuuid}] ->
                                        {false, Cuuid}
                                end,
                            ErrorOrId(Unique, CId)
                    end,
    Result = mnesia:transaction(AddCredential),
    convert_result(Result).



%% @doc get the list of credentials for a user
%% A callback from the watts_persistent behaviour.
-spec credential_get_list(UserId::binary()) -> {ok, [watts:credential()]}.
credential_get_list(UserId) ->
    MatchSpec = [{#credential{user_id=UserId, _='_'}, [],
                  ['$_']}],
    GetList =
        fun() ->
                mnesia:select(credential, MatchSpec, read)

        end,
    ToCred = fun(#credential{uuid = CredId, ctime= CTime,
                             interface = Interface, service_id = ServiceId,
                             state = CredState}, List) ->
                     Cred = #{cred_id => CredId, ctime => CTime,
                              interface => Interface, service_id => ServiceId,
                              cred_state => CredState},
                     [Cred | List]
             end,
    Result = case mnesia:transaction(GetList) of
                 {atomic, CredList} ->
                     {ok, lists:reverse(lists:foldl(ToCred, [], CredList))};
                 Other ->
                     Other
             end,
    convert_result(Result).

%% @doc get the number of credentials for a user at a service.
%% A callback from the watts_persistent behaviour.
-spec credential_get_count(UserId::binary(), ServiceId::binary()) ->
                                  {ok, pos_integer()}.
credential_get_count(UserId, ServiceId) ->
    MatchSpec = [{#credential{user_id=UserId, service_id=ServiceId,
                              uuid='$1', _='_'}, [], ['$1']}],
    GetList =
        fun() ->
                {ok, length(mnesia:select(credential, MatchSpec, read))}
        end,
    convert_result(mnesia:transaction(GetList)).


%% @doc get a specific credential.
%% A callback from the watts_persistent behaviour.
-spec credential_get(CredId::binary())
                    -> {ok, watts:credential()} | {error, Reason::atom()}.
credential_get(Uuid) ->
    Get =
        fun() ->
                mnesia:select(credential, match_spec(Uuid, undefined), read)
        end,
    ToCred = fun(#credential{uuid = CredId, ctime= CTime, user_id=Userid,
                             interface = Interface, service_id = ServiceId,
                             state = CredState}) ->
                     #{cred_id => CredId, ctime => CTime, user_id => Userid,
                              interface => Interface, service_id => ServiceId,
                              cred_state => CredState}
             end,
    Result = case mnesia:transaction(Get) of
                 {atomic, [Cred]} ->
                     {ok, ToCred(Cred)};
                 {atomic, []} ->
                     {error, not_found};
                 {atomic, _} ->
                     {error, too_many};
                 Other ->
                     Other
             end,
    convert_result(Result).



%% @doc remove a credential of a user
%% A callback from the watts_persistent behaviour.
-spec credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.
credential_remove(UserId, CredId) ->
    MatchSpec = match_spec(CredId, UserId),
    Delete =
        fun() ->
                case mnesia:select(credential, MatchSpec, write) of
                    [] -> ok;
                    [Cred] ->
                        Id = Cred#credential.id,
                        ok = mnesia:delete(credential, Id, write),
                        ok
                end
        end,
    Result = mnesia:transaction(Delete),
    convert_result(Result).



%% @doc return if the database is ready.
%% A callback from the watts_persistent behaviour.
-spec is_ready() -> true | {false, Reason :: atom()}.
is_ready() ->
    case mnesia:table_info(credential, type) of
        set -> true;
        _ ->
            {false, not_running}
    end.


%% @doc create the schema for the mnesia database if needed
-spec setup_mnesia_if_needed(boolean()) -> ok.
setup_mnesia_if_needed(true) ->
    ok;
setup_mnesia_if_needed(false) ->
    lager:info("Init: creating mnesia schema and tables."),
    ok = mnesia:create_schema([node()]),
    ok = application:start(mnesia),
    ok = create_tables(),
    ok.

%% @doc create the needed tables
-spec create_tables() -> ok.
create_tables() ->
    Nodes = [ node() | nodes() ],
    {atomic, ok} =
        mnesia:create_table(credential,
                            [{disc_copies, Nodes},
                             {index, [uuid, user_id, service_id, state]},
                             {attributes, record_info(fields, credential)}
                            ]),
    ok.

%% @doc stop the mnesia application
-spec stop_mnesia() -> ok.
stop_mnesia() ->
    application:stop(mnesia),
    application:set_env(mnesia, dir, ?CONFIG(mnesia_dir)),
    ok.

%% @doc start the mnesia application
-spec start_mnesia() -> ok.
start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    ok.

%% @doc create a random uuid for the credential
-spec create_random_uuid() -> binary().
create_random_uuid() ->
    Uuid = list_to_binary(uuid:uuid_to_string(uuid:get_v4(strong))),
    case mnesia:select(credential, match_spec(Uuid, undefined), write) of
        [] ->
            Uuid;
        _ ->
            create_random_uuid()
    end.

%% @doc match specifications to search within the mnesia database
-spec match_spec(CredentialUuid :: binary(),
                 UserId :: binary() | undefined) -> [tuple()].
match_spec(Uuid, undefined) ->
    [{#credential{uuid=Uuid, _='_'}, [], ['$_']}];
match_spec(Uuid, UserId) ->
    [{#credential{uuid=Uuid, user_id = UserId, _='_'}, [], ['$_']}].


%% @doc convert the result to a common return value
-spec convert_result(tuple()) -> {ok, any()} | {error, Reason :: atom()}.
convert_result({atomic, Result}) ->
    Result;
convert_result({ok, Result}) ->
    {ok, Result};
convert_result({error, Reason}) ->
    {error, Reason};
convert_result({aborted, Reason}) ->
    {error, Reason}.
