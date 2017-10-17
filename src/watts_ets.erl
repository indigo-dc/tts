%% @doc This module implements storage and lookup of data in ETS tables
-module(watts_ets).
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
-include("watts.hrl").

-export([init/0,
         destroy/0
        ]).

-export([
         sessions_get_list/0,
         sessions_create_new/1,
         sessions_get_pid/1,
         sessions_update_pid/2,
         sessions_delete/1
        ]).

-export([
         service_add/2,
         service_update/2,
         service_get/1,
         service_get_list/0
        ]).

-define(WATTS_SESSIONS, watts_sessions).
-define(WATTS_OIDCP, watts_oidcp).
-define(WATTS_SERVICE, watts_service).

%% the list of tables to automatically create/delete ... etc
-define(WATTS_TABLES, [
                     ?WATTS_SESSIONS
                    , ?WATTS_OIDCP
                    , ?WATTS_SERVICE
                    ]).

%% @doc create the ets tables
-spec init() -> ok.
init() ->
    create_tables().

%% @doc delete all ets tables
-spec destroy() -> ok.
destroy() ->
    delete_tables().


% functions for session management
%% @doc get the list of all sessions
-spec sessions_get_list() -> [map()].
sessions_get_list() ->
    Entries = get_all_entries(?WATTS_SESSIONS),
    ExtractValue = fun({Id, Pid}, List) ->
                           [#{id => Id, pid => Pid} | List]
                   end,
    lists:reverse(lists:foldl(ExtractValue, [], Entries)).


%% @doc create a new session entry, without a pid.
-spec sessions_create_new(Token :: binary()) -> ok | {error, Reason :: atom()}.
sessions_create_new(Token) ->
    return_ok_or_error(insert_new(?WATTS_SESSIONS, {Token, none_yet})).


%% @doc return the pid for a given token or error
-spec sessions_get_pid(Token :: binary()) -> {ok, Pid :: pid()} |
                                          {error, Reason :: atom()}.
sessions_get_pid(Token) ->
    case return_value(lookup(?WATTS_SESSIONS, Token)) of
        {ok, none_yet} -> {error, none_yet};
        Other -> Other
    end.

%% @doc update the pid for the given token
-spec sessions_update_pid(Token :: binary(), Pid :: pid()) -> ok.
sessions_update_pid(Token, Pid) ->
    insert(?WATTS_SESSIONS, {Token, Pid}),
    ok.

%% @doc delete the session entry for the given token
-spec sessions_delete(Token :: binary()) -> ok.
sessions_delete(Token) ->
    delete(?WATTS_SESSIONS, Token).


% functions for service  management
%% @doc add a service with the given Identifier and its Config
-spec service_add(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_add(Identifier, Info) ->
    return_ok_or_error(insert_new(?WATTS_SERVICE, {Identifier, Info})).

%% @doc update the Config of the given ServiceId.
-spec service_update(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_update(Identifier, Info) ->
    return_ok_or_error(insert(?WATTS_SERVICE, {Identifier, Info})).

%% @doc lookup a service by its id
-spec service_get(Identifier::binary()) ->
                         {ok, tuple()} | {error, Reason :: atom()}.
service_get(Id) ->
    lookup(?WATTS_SERVICE, Id).

%% @doc get a list of all service configs
-spec service_get_list() -> {ok, [map()]}.
service_get_list() ->
    Entries = get_all_entries(?WATTS_SERVICE),
    ExtractValue = fun({_, Val}, List) ->
                           [Val | List]
                   end,
    {ok, lists:reverse(lists:foldl(ExtractValue, [], Entries))}.

%% internal functions

%% @doc convert booelan to ok/{error, reason}
-spec return_ok_or_error(boolean()) -> ok | {error, already_exists}.
return_ok_or_error(true) ->
    ok;
return_ok_or_error(false) ->
    {error, already_exists}.

%% @doc unify return value
-spec return_value({ok, {Key :: any(), Value::any()}} |
                   {error, any()}
                  )
                  -> {ok, Value :: any()} | {error, any()}.
return_value({ok, {_Key, Value}}) ->
    {ok, Value};
return_value({error, _} = Error) ->
    Error.

%% @doc create the ets tables
-spec create_tables() -> ok.
create_tables() ->
    CreateTable = fun(Table) ->
                          create_table(Table)
                  end,
    lists:map(CreateTable, ?WATTS_TABLES),
    ok = wait_for_tables(?WATTS_TABLES),
    ok.

%% @doc create a single ets table with name TableName
-spec create_table(Name :: atom()) -> ets:tid() | atom().
create_table(TableName) ->
    Heir = case erlang:whereis(watts_sup) of
              undefined -> {heir, none};
              Pid -> {heir, Pid, none}
          end,
    ets:new(TableName, [set, public, named_table, {keypos, 1}, Heir]).


%% @doc wait until the tables are ready
-spec wait_for_tables([atom()]) -> ok.
wait_for_tables(List) ->
    Check = fun(Table, Result) ->
                     case ets:info(Table) of
                         undefined ->
                             false;
                         _ ->
                             Result
                     end
             end,
    case lists:foldl(Check, true, List) of
        false ->
            timer:sleep(100),
            wait_for_tables(List);
        true ->
            ok
    end.

%% @doc delete all tables
-spec delete_tables() -> ok.
delete_tables() ->
    DeleteTable = fun(Table) ->
                          delete_table(Table)
                  end,
    lists:map(DeleteTable, ?WATTS_TABLES),
    ok.

%% @doc delete a single table
-spec delete_table(Name :: atom()) -> ok.
delete_table(Name) ->
    case ets:info(Name) of
        undefined ->
            ok;
        _ ->
            true = ets:delete(Name),
            ok
    end.

%% @doc get all entries of a table
-spec get_all_entries(Table :: atom()) -> [any()].
get_all_entries(Table) ->
    GetVal = fun(Entry, List) ->
                     [Entry | List]
             end,
    Entries = ets:foldl(GetVal, [], Table),
    lists:reverse(Entries).

%% @doc delete the specified Entry.
-spec delete(Table :: atom(), Key :: any()) -> ok.
delete(Table, Key) ->
    true = ets:delete(Table, Key),
    ok.

%% @doc insert an entry into the table
-spec insert(Table :: atom(), Entry :: any()) -> true.
insert(Table, Entry) ->
    true = ets:insert(Table, Entry).

%% @doc insert an new entry into the table
-spec insert_new(Table :: atom(), Entry :: any()) -> boolean().
insert_new(Table, Entry) ->
    ets:insert_new(Table, Entry).

%% @doc lookup a key in the given table
-spec lookup(Table :: atom(), Key :: any()) ->
                    {ok, Element::any()} | {error, not_found}.
lookup(Table, Key) ->
    create_lookup_result(ets:lookup(Table, Key)).

%% @doc convert the lookup result into ok/error tuple
-spec create_lookup_result([any()]) -> {ok, any()} |
                                       {error, not_found}.
create_lookup_result([Element]) ->
    {ok, Element};
create_lookup_result([]) ->
    {error, not_found}.
