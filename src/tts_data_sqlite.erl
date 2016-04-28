-module(tts_data_sqlite).
-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([reconfigure/0]).
-export([credential_add/4]).
-export([credential_get/1]).
-export([credential_get_list/1]).
-export([credential_get_count/2]).
-export([credential_remove/2]).


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

-spec reconfigure() -> ok.
reconfigure() ->
    gen_server:cast(?MODULE, reconfigure).

-spec credential_add(UserId::binary(), ServiceId::binary(),
                     Interface ::binary(), CredState :: any())
-> ok | {error, Reason :: atom()}.
credential_add(UserId, ServiceId, Interface, CredState) ->
    gen_server:call(?MODULE, {credential_add, UserId, ServiceId, Interface,
                              CredState}).

-spec credential_get_list(UserId::binary()) -> [tts:cred()].
credential_get_list(UserId) ->
    gen_server:call(?MODULE, {credential_get_list, UserId}).

-spec credential_get_count(UserId::binary(), ServiceId::binary()) -> integer().
credential_get_count(UserId, ServiceId) ->
    gen_server:call(?MODULE, {credential_get_count, UserId, ServiceId}).

-spec credential_get(CredId::binary()) -> {ok, tts:cred()}.
credential_get(CredId) ->
    gen_server:call(?MODULE, {credential_get, CredId}).

-spec credential_remove(UserId::binary(), CredentialId::binary()) ->
    ok | {error, Reason :: atom()}.
credential_remove(UserId, CredId) ->
    gen_server:call(?MODULE, {credential_remove, UserId, CredId}).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, #state{con=undefined}=State) ->
    {reply, {error, not_configured}, State};
handle_call({credential_add, UserId, ServiceId, Interface, CredState}, _From
            , #state{con=Con}=State) ->
    ok = credential_add(UserId, ServiceId, Interface, CredState, Con),
    {reply, ok, State};
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
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(reconfigure, State) ->
    NewState = reconfigure(State),
    {noreply, NewState};
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


credential_add(UserId, ServiceId, Interface, CredState, Con) ->
    ok = esqlite3:exec("begin;", Con),
    CredentialId = create_random_credential_id(Con),
    CreationTime = erlang:system_time(seconds),
    esqlite3:q("INSERT INTO tts_cred VALUES( ?1, ?2, ?3, ?4, ?5, ?6);"
               , [CredentialId, CreationTime, Interface, UserId, ServiceId,
                  CredState] , Con),
    ok = esqlite3:exec("commit;", Con),
    ok.


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

credential_remove(UserId, CredentialId, Con) ->
    ok = esqlite3:exec("begin;", Con),
    esqlite3:q("DELETE FROM tts_cred WHERE user_id=?1 AND credential_id=?2;",
                [UserId, CredentialId], Con),
    ok = esqlite3:exec("commit;", Con),
    ok.

create_random_credential_id(Con) ->
    CredId = tts_utils:random_string(24),
    Result = esqlite3:q("SELECT credential_id FROM tts_cred WHERE
                        credential_id = ?1", [CredId], Con),
    case Result of
        [] -> CredId;
        _ -> create_random_credential_id(Con)
    end.


reconfigure(#state{con=undefined}) ->
    {ok, Con} = esqlite3:open(?CONFIG(sqlite_db)),
    ok = create_tables_if_needed(Con),
    #state{con=Con};
reconfigure(#state{con=Con} = State) ->
    esqlite3:close(Con),
    reconfigure(State#state{con=undefined}).

-define(TABLES, [
                 {cred,
                  <<"tts_cred">>,
                  <<"CREATE TABLE tts_cred(credential_id Text, ctime INTEGER,
                  interface TEXT, user_id TEXT, service_id TEXT,
                    credstate TEXT)">>}
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
