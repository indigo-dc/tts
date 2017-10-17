-module(watts_persistent_sqlite).
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
-behaviour(watts_persistent).
-include("watts.hrl").

%% API.
-export([start_link/0]).
-export([initialize/0]).
-export([reconfigure/0]).
-export([credential_add/5]).
-export([credential_get/1]).
-export([credential_get_list/1]).
-export([credential_get_count/2]).
-export([credential_remove/2]).
-export([is_ready/0]).

-export([stop/0]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          con = undefined
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


initialize() ->
    lager:info("Init: starting sqlite database ~p", [?CONFIG(sqlite_db)]),
    reconfigure().

-spec reconfigure() -> ok.
reconfigure() ->
    gen_server:cast(?MODULE, reconfigure).

-spec credential_add(UserId::binary(), ServiceId::binary(),
                     Interface ::binary(), CredState :: any(),
                     AllowNonUniqueStates::boolean())
-> {ok, CredentialID ::binary()} | {error, Reason :: atom()}.
credential_add(UserId, ServiceId, Interface, CredState, SameStateAllowed) ->
    gen_server:call(?MODULE, {credential_add, UserId, ServiceId, Interface,
                              CredState, SameStateAllowed}).

-spec credential_get_list(UserId::binary()) -> {ok, [watts:cred()]}.
credential_get_list(UserId) ->
    gen_server:call(?MODULE, {credential_get_list, UserId}).

-spec credential_get_count(UserId::binary(), ServiceId::binary()) ->
                                  {ok, pos_integer()}.
credential_get_count(UserId, ServiceId) ->
    gen_server:call(?MODULE, {credential_get_count, UserId, ServiceId}).

-spec credential_get(CredId::binary()) -> {ok, watts:cred()}.
credential_get(CredId) ->
    gen_server:call(?MODULE, {credential_get, CredId}).

-spec credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.
credential_remove(UserId, CredId) ->
    gen_server:call(?MODULE, {credential_remove, UserId, CredId}).

-spec is_ready() -> true | {false, Reason :: atom()}.
is_ready() ->
    gen_server:call(?MODULE, is_ready).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, #state{con=undefined}=State) ->
    {reply, {error, not_configured}, State};
handle_call({credential_add, UserId, ServiceId, Interface, CredState,
             SameStateAllowed}, _From , #state{con=Con}=State) ->
    Result = credential_add(UserId, ServiceId, Interface, CredState,
                            SameStateAllowed, Con),
    {reply, Result, State};
handle_call({credential_get_list, UserId}, _From, #state{con=Con}=State) ->
    CredList = credential_get_list(UserId, Con),
    {reply, {ok, CredList}, State};
handle_call({credential_get_count, UserId, ServiceId}, _From,
            #state{con=Con}=State) ->
    Count = credential_get_count(UserId, ServiceId, Con),
    {reply, {ok, Count}, State};
handle_call({credential_get, CredId}, _From, #state{con=Con}=State) ->
    CredOrError = credential_get(CredId, Con),
    {reply, CredOrError, State};
handle_call({credential_remove, UserId, CredentialId}, _From
            , #state{con=Con}=State) ->
    ok = credential_remove(UserId, CredentialId, Con),
    {reply, ok, State};
handle_call(is_ready, _From, #state{con=Con} = State) ->
    Result = case Con of
                 undefined ->
                     {false, not_ready};
                 _ ->
                      true
             end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(reconfigure, State) ->
    NewState = reconfigure(?CONFIG(sqlite_db, undefined), State),
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{con=undefined}) ->
    ok;
terminate(_Reason, #state{con=Con}) ->
    esqlite3:close(Con),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


credential_add(UserId, ServiceId, Interface, CredState, SameStateOk, Con) ->
    ok = esqlite3:exec("begin;", Con),
    {Unique, CUuid}=credential_state_unique(UserId, ServiceId, CredState, Con),
    Result = case Unique of
                 true ->
                     CredUuid = create_random_uuid(Con),
                     CreationTime = cowboy_clock:rfc1123(),
                     esqlite3:q("INSERT INTO tts_cred
                                VALUES( ?1,?2,?3,?4,?5,?6);",
                                [CredUuid, CreationTime, Interface, UserId,
                                 ServiceId, CredState] , Con),
                     {ok, CredUuid};
                 false ->
                     case SameStateOk of
                         true ->
                             {ok, CUuid};
                         _ ->
                             {error, not_unique_state}
                     end
             end,
    ok = esqlite3:exec("commit;", Con),
    Result.


credential_get_list(UserId, Con) ->
    CredList = esqlite3:q("SELECT credential_id, ctime, interface, service_id,
                           credstate FROM tts_cred WHERE  user_id IS ?"
                          , [UserId], Con),
    ToCred = fun({CredId, CTime, Interface, ServiceId, CredState}, List) ->
                     Cred = #{cred_id => CredId, ctime => CTime,
                       interface => Interface, service_id => ServiceId,
                       cred_state => CredState},
                     [Cred | List]
             end,
    lists:reverse(lists:foldl(ToCred, [], CredList)).

credential_get_count(UserId, ServiceId, Con) ->
    [{Count}] = esqlite3:q("SELECT COUNT(user_id) FROM tts_cred WHERE
                          user_id IS ?1 AND service_id IS ?2"
                          , [UserId, ServiceId], Con),
    Count.

credential_get(CredId, Con) ->
    Result = esqlite3:q("SELECT credential_id, ctime, interface, service_id,
                         credstate, user_id FROM tts_cred
                         WHERE  credential_id IS ?"
                          , [CredId], Con),
    ToCred = fun({Id, CTime, Interface, ServiceId, CredState, UserId}) ->
                     #{cred_id => Id, ctime => CTime,
                       interface => Interface, service_id => ServiceId,
                       cred_state => CredState, user_id => UserId}
             end,
    case Result of
        [] -> {error, not_found};
        [Cred] -> {ok, ToCred(Cred)}
    end.

credential_state_unique(UserId, ServiceId, CredState, Con) ->
    Result = esqlite3:q("SELECT credential_id FROM tts_cred
                         WHERE user_id IS ?1 AND service_id IS ?2
                         AND credstate IS ?3"
                          , [UserId, ServiceId, CredState], Con),
    case Result of
        [] -> {true, none};
        [{CredId}] -> {false, CredId}
    end.


credential_remove(UserId, CredentialId, Con) ->
    ok = esqlite3:exec("begin;", Con),
    esqlite3:q("DELETE FROM tts_cred WHERE user_id=?1 AND credential_id=?2;",
                [UserId, CredentialId], Con),
    ok = esqlite3:exec("commit;", Con),
    ok.

create_random_uuid(Con) ->
    Uuid = list_to_binary(uuid:uuid_to_string(uuid:get_v4(strong))),
    Result = esqlite3:q("SELECT credential_id FROM tts_cred WHERE
                        credential_id = ?1", [Uuid], Con),
    case Result of
        [] -> Uuid;
        _ -> create_random_uuid(Con)
    end.



reconfigure(undefined, #state{con=undefined} = State) ->
    State;
reconfigure(NewDB, #state{con=undefined}) ->
    case esqlite3:open(NewDB) of
        {ok, Con} ->
            ok = create_tables_if_needed(Con),
            #state{con=Con};
        {error, Reason} ->
            lager:critical("error opening the sqlite database ~p", [Reason]),
            #state{con=undefined}
    end;
reconfigure(NewDB, #state{con=Con} = State) ->
    esqlite3:close(Con),
    reconfigure(NewDB, State#state{con=undefined}).

-define(TABLES, [
                 {cred,
                  <<"tts_cred">>,
                  <<"CREATE TABLE tts_cred(credential_id Text, ctime TEXT,
                  interface TEXT, user_id TEXT, service_id TEXT,
                    credstate TEXT);
                    CREATE INDEX tts_cred_user_idx ON tts_cred(user_id);
                    CREATE INDEX tts_cred_service_idx ON tts_cred(service_id);
                    CREATE INDEX tts_cred_state_idx ON tts_cred(credstate);
                    ">>}
                ]).

create_tables_if_needed(Con) ->
    ok = esqlite3:exec("begin;", Con),
    CreateTable = fun({_, Name, Create}, _) ->
                          Select = << <<"SELECT * FROM ">>/binary,
                                      Name/binary >>,
                          case esqlite3:exec(Select, Con) of
                              {error, {sqlite_error, _}} ->
                                  ok = esqlite3:exec(Create, Con),
                                  ok;
                              _ ->
                                  ok
                          end
                  end,
    ok = lists:foldl(CreateTable, ok, ?TABLES),
    ok = esqlite3:exec("commit;", Con),
    ok.
