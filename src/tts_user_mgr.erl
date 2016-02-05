-module(tts_user_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([get_user/2]).

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

-spec get_user(UserSuject :: binary(), Issuer :: binary()) -> {ok, pid()} | {error, term()}.
get_user(UserSubject, Issuer) ->
    load_or_return_user({UserSubject, Issuer}).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({create,User}, _From, State) ->
    Result = create_new_user(User),
    {reply, Result, State};
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



load_or_return_user(User) ->
    load_or_return_user(lookup_user(User),User).

load_or_return_user({ok, Pid}, _User) ->
    {ok, Pid};
load_or_return_user({error, not_found}, User) ->
    {ok, UserProcess} = create_user(User),
    {ok, Data} = retrieve_user_data(User),
    update_user_with_data(Data,UserProcess);
load_or_return_user(Error, _User) ->
    Error.


create_user(User) ->
    gen_server:call(?MODULE,{create,User}).

create_new_user(User) ->
    {ok, Pid} = tts_user_sup:add_user(User),
    case add_new_user_entry(User,Pid) of
        true -> 
            {ok, Pid};
        false ->
            ok = tts_user:stop(Pid),
            lookup_user(User)
    end.

retrieve_user_data(_User) ->
    %TODO: implement
    {ok, #{user => <<"guest">>, uid => 1111, gid => 1111}}.

update_user_with_data(_Map, UserPid)  ->
    %TODO: implement
    {ok, UserPid}.
    

%% 
%% functions with data access
%%

add_new_user_entry(ID,Pid) ->
   tts_data:user_create_new(ID,Pid). 

lookup_user(ID) ->
   tts_data:user_get_pid(ID). 

%% set_pid_for_id(ID,Pid) ->
%%    tts_data:user_update_pid(ID,Pid). 

%% delete_session(ID) ->
%%     tts_data:user_delete(ID),
%%     ok.
