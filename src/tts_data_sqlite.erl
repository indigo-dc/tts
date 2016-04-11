-module(tts_data_sqlite).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([reconfigure/0]).
-export([credential_add/3]).
-export([credential_get/1]).
-export([credential_remove/3]).


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
                     CredState :: any()) -> ok | {error, Reason :: atom()}.
credential_add(UserId, ServiceId, CredState) ->
    gen_server:call(?MODULE, {credential_add, UserId, ServiceId, CredState}).

-spec credential_get(UserId::binary()) ->ok.
credential_get(UserId) ->
    gen_server:call(?MODULE, {credential_get, UserId}).

-spec credential_remove(UserId::binary(), ServiceId::binary(),
                       CredState :: any()) ->ok | {error, Reason :: atom()}.
credential_remove(UserId, ServiceId, CredState) ->
    gen_server:call(?MODULE, {credential_remove, UserId, ServiceId, CredState}).

%% gen_server.
-include("tts.hrl").

init([]) ->
    {ok, #state{}}.

handle_call(_, _From, #state{con=undefined}=State) ->
    {reply, {error, not_configured}, State};
handle_call({credential_add, UserId, ServiceId, CredState}, _From
            , #state{con=Con}=State) ->
    ok = credential_add(UserId, ServiceId, CredState, Con),
    {reply, ok, State};
handle_call({credential_get, UserId}, _From, #state{con=Con}=State) ->
    CredList = credential_get(UserId, Con),
    {reply, {ok, CredList}, State};
handle_call({credential_remove, UserId, ServiceId, CredState}, _From
            , #state{con=Con}=State) ->
    ok = credential_remove(UserId, ServiceId, CredState, Con),
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


credential_add(UserId, ServiceId, CredState, Con) ->
    ok = esqlite3:exec("begin;", Con),
    esqlite3:q("INSERT INTO tts_cred VALUES( ?1, ?2, ?3);"
               , [UserId, ServiceId, CredState], Con),
    ok = esqlite3:exec("commit;", Con),
    ok.


credential_get(UserId, Con) ->
    esqlite3:q("SELECT serviceid, credstate FROM tts_cred WHERE  userid IS ?"
               , [UserId], Con).

credential_remove(UserId, ServiceId, CredState, Con) ->
    ok = esqlite3:exec("begin;", Con),
    esqlite3:q("DELETE FROM tts_cred WHERE userid = ?1 AND serviceid = ?2 AND
               credstate = ?3;", [UserId,
                                 ServiceId,
                                 CredState], Con),
    ok = esqlite3:exec("commit;", Con),
    ok.

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
                  <<"CREATE TABLE tts_cred(userid TEXT,
                    serviceid TEXT, credstate TEXT)">>}
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
