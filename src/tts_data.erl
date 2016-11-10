-module(tts_data).
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
-include("tts.hrl").

-export([init/0]).

-export([
         sessions_get_list/0,
         sessions_count/0,
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

-define(TTS_SESSIONS, tts_sessions).
-define(TTS_OIDCP, tts_oidcp).
-define(TTS_SERVICE, tts_service).

-define(TTS_TABLES, [
                     ?TTS_SESSIONS
                    , ?TTS_OIDCP
                    , ?TTS_SERVICE
                    ]).

init() ->
    create_tables().

% functions for session management
-spec sessions_get_list() -> [map()].
sessions_get_list() ->
    Entries = get_all_entries(?TTS_SESSIONS),
    ExtractValue = fun({Id, Pid}, List) ->
                           [#{id => Id, pid => Pid} | List]
                   end,
    lists:reverse(lists:foldl(ExtractValue, [], Entries)).

-spec sessions_count() -> integer.
sessions_count() ->
    get_num_entries(?TTS_SESSIONS).



-spec sessions_create_new(Token :: binary()) -> ok | {error, Reason :: atom()}.
sessions_create_new(Token) ->
    return_ok_or_error(insert_new(?TTS_SESSIONS, {Token, none_yet})).


-spec sessions_get_pid(Token :: binary()) -> {ok, Pid :: pid()} |
                                          {error, Reason :: atom()}.
sessions_get_pid(Token) ->
    case return_value(lookup(?TTS_SESSIONS, Token)) of
        {ok, none_yet} -> {error, none_yet};
        Other -> Other
    end.

-spec sessions_update_pid(Token :: binary(), Pid :: pid()) -> ok.
sessions_update_pid(Token, Pid) ->
    insert(?TTS_SESSIONS, {Token, Pid}),
    ok.

-spec sessions_delete(Token :: binary()) -> true.
sessions_delete(Token) ->
    delete(?TTS_SESSIONS, Token).


% functions for service  management
-spec service_add(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_add(Identifier, Info) ->
    return_ok_or_error(insert_new(?TTS_SERVICE, {Identifier, Info})).

-spec service_update(Identifier::binary(), Info :: map()) ->
    ok | {error, Reason :: atom()}.
service_update(Identifier, Info) ->
    return_ok_or_error(insert(?TTS_SERVICE, {Identifier, Info})).

-spec service_get(Identifier::binary()) ->ok.
service_get(Id) ->
    lookup(?TTS_SERVICE, Id).

-spec service_get_list() -> {ok, [map()]}.
service_get_list() ->
    Entries = get_all_entries(?TTS_SERVICE),
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
return_value({ok, {_Key, Value, _ATime, _CTime}}) ->
    {ok, Value};
return_value({error, _} = Error) ->
    Error.


create_tables() ->
    CreateTable = fun(Table) ->
                          create_table(Table)
                  end,
    lists:map(CreateTable, ?TTS_TABLES),
    ok.

create_table(TableName) ->
    ets:new(TableName, [set, public, named_table, {keypos, 1}]).


get_num_entries(Table) ->
    ets:info(Table, size).

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
    {error, not_found};
create_lookup_result(_) ->
    {error, too_many}.
