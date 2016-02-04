-module(tts_user_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([get_user/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_user(UserId :: binary()) -> {ok, pid()} | {error, term()}.
get_user(UserId) ->
    load_or_return_user(UserId).

%% gen_server.

init([]) ->
	{ok, #state{}}.

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



load_or_return_user(Id) ->
    load_or_return_user(lookup_user(Id),Id).

load_or_return_user({ok, Pid}, _UserId) ->
    {ok, Pid};
load_or_return_user({error, not_found}, UserId) ->
    load_user(UserId);
load_or_return_user(Error, _UserId) ->
    Error.


load_user(_UserId) ->
    ok.




%% 
%% functions with data access
%%

%% add_new_user_entry(ID,Pid) ->
%%    tts_data:user_create_new(ID,Pid). 

lookup_user(ID) ->
   tts_data:user_get_pid(ID). 

%% set_pid_for_id(ID,Pid) ->
%%    tts_data:user_update_pid(ID,Pid). 

%% delete_session(ID) ->
%%     tts_data:user_delete(ID),
%%     ok.
