-module(watts_data).
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

-define(WATTS_TABLES, [
                     ?WATTS_SESSIONS
                    , ?WATTS_OIDCP
                    , ?WATTS_SERVICE
                    ]).

init() ->
    create_tables().

destroy() ->
    delete_tables().

% functions for session management
-spec sessions_get_list() -> [map()].
sessions_get_list() ->
    Entries = get_all_entries(?WATTS_SESSIONS),
    ExtractValue = fun({Id, Pid}, List) ->
                           [#{id => Id, pid => Pid} | List]
                   end,
    lists:reverse(lists:foldl(ExtractValue, [], Entries)).


-spec sessions_create_new(Token :: binary()) -> ok | {error, Reason :: atom()}.
sessions_create_new(Token) ->
    return_ok_or_error(insert_new(?WATTS_SESSIONS, {Token, none_yet})).


-spec sessions_get_pid(Token :: binary()) -> {ok, Pid :: pid()} |
                                          {error, Reason :: atom()}.
sessions_get_pid(Token) ->
    case return_value(lookup(?WATTS_SESSIONS, Token)) of
        {ok, none_yet} -> {error, none_yet};
        Other -> Other
    end.

-spec sessions_update_pid(Token :: binary(), Pid :: pid()) -> ok.
sessions_update_pid(Token, Pid) ->
    insert(?WATTS_SESSIONS, {Token, Pid}),
    ok.

-spec sessions_delete(Token :: binary()) -> ok.
sessions_delete(Token) ->
    delete(?WATTS_SESSIONS, Token).


% functions for service  management
-spec service_add(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_add(Identifier, Info) ->
    return_ok_or_error(insert_new(?WATTS_SERVICE, {Identifier, Info})).

-spec service_update(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_update(Identifier, Info) ->
    return_ok_or_error(insert(?WATTS_SERVICE, {Identifier, Info})).

-spec service_get(Identifier::binary()) ->
                         {ok, tuple()} | {error, Reason :: atom()}.
service_get(Id) ->
    lookup(?WATTS_SERVICE, Id).

-spec service_get_list() -> {ok, [map()]}.
service_get_list() ->
    Entries = get_all_entries(?WATTS_SERVICE),
    ExtractValue = fun({_, Val}, List) ->
                           [Val | List]
                   end,
    {ok, lists:reverse(lists:foldl(ExtractValue, [], Entries))}.

%% internal functions


return_ok_or_error(true) ->
    ok;
return_ok_or_error(false) ->
    {error, already_exists}.

return_value({ok, {_Key, Value}}) ->
    {ok, Value};
return_value({error, _} = Error) ->
    Error.


create_tables() ->
    CreateTable = fun(Table) ->
                          create_table(Table)
                  end,
    lists:map(CreateTable, ?WATTS_TABLES),
    ok = wait_for_tables(?WATTS_TABLES),
    ok.

create_table(TableName) ->
    Heir = case erlang:whereis(watts_sup) of
              undefined -> {heir, none};
              Pid -> {heir, Pid, none}
          end,
    ets:new(TableName, [set, public, named_table, {keypos, 1}, Heir]).


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


delete_tables() ->
    DeleteTable = fun(Table) ->
                          delete_table(Table)
                  end,
    lists:map(DeleteTable, ?WATTS_TABLES),
    ok.

delete_table(Name) ->
    case ets:info(Name) of
        undefined ->
            ok;
        _ ->
            ets:delete(Name)
    end.

get_all_entries(Table) ->
    GetVal = fun(Entry, List) ->
                     [Entry | List]
             end,
    Entries = ets:foldl(GetVal, [], Table),
    lists:reverse(Entries).

delete(Table, Key) ->
    true = ets:delete(Table, Key),
    ok.


insert(Table, Entry) ->
    true = ets:insert(Table, Entry).

insert_new(Table, Entry) ->
    ets:insert_new(Table, Entry).

lookup(Table, Key) ->
    create_lookup_result(ets:lookup(Table, Key)).

create_lookup_result([Element]) ->
    {ok, Element};
create_lookup_result([]) ->
    {error, not_found}.
