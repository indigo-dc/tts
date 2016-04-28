-module(tts_rest_cred).
-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([add_cred/2]).
-export([get_cred/2]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          creds=[]
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_cred(Credential, UserId) ->
    gen_server:call(?MODULE, {add, Credential, UserId}).

get_cred(Id, UserId) ->
    gen_server:call(?MODULE, {get, Id, UserId}).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({add,  Cred, UserId}, _From, #state{creds = Creds} = State) ->
    Id = gen_random_id(State),
    NewCreds = [{Id, UserId, Cred} | Creds],
    {reply, {ok, Id}, State#state{creds=NewCreds}};
handle_call({get, Id, UserId}, _From, #state{creds=Creds}=State) ->
    case lists:keyfind(Id, 1, Creds) of
        {Id, UserId, Cred} ->
            NewCreds = lists:keydelete(Id, 1, Creds),
            {reply, {ok, Cred}, State#state{creds=NewCreds}};
        _ ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


gen_random_id(#state{creds = Creds} = State) ->
    Id = tts_utils:random_string(16),
    case lists:keyfind(Id, 1, Creds) of
        false -> Id;
        _ -> gen_random_id(State)
    end.
