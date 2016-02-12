-module(tts_user_mgr).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_user/2]).
-export([user_wants_to_shutdown/1]).

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
    load_and_return_user(#{ sub => UserSubject, iss => Issuer}).

-spec user_wants_to_shutdown(ID :: binary()) -> ok.
user_wants_to_shutdown(ID) ->
    gen_server:call(?MODULE,{user_wants_to_shutdown,ID}).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({create,UserId}, _From, State) ->
    Result = create_new_user(UserId),
    {reply, Result, State};
handle_call({user_wants_to_shutdown,ID}, _From, State) ->
    {ok,Pid} = lookup_user(ID),
    true = delete_user(ID),
    ok = tts_user:shutdown(Pid),
    {reply, ok, State};
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



load_and_return_user(UserMap) ->
    UserInfo = tts_idh:lookup_user(UserMap),
    create_user(UserInfo).

create_user({ok, UserId, UserInfo}) ->
    create_or_return_pid(lookup_user(UserId),UserId,UserInfo);
create_user({error, not_found}) ->
    {error, not_found}.

create_or_return_pid({ok,Pid},_UserId,_UserInfo)->
    {ok, Pid};
create_or_return_pid({error, not_found},UserId,UserInfo) ->
    {ok, UserPid} = gen_server:call(?MODULE,{create,UserId}),
    ok = tts_user:set_user_info(UserInfo,UserPid),
    {ok, UserPid};
create_or_return_pid(Error, _UserId, _UserInfo) ->
    Error.



create_new_user(UserId) ->
    {ok, Pid} = tts_user_sup:add_user(UserId),
    case add_new_user_entry(UserId,Pid) of
        ok -> 
            {ok, Pid};
        {error, already_exists} ->
            ok = tts_user:stop(Pid),
            lookup_user(UserId)
    end.



%% 
%% functions with data access
%%

add_new_user_entry(ID,Pid) ->
   tts_data:user_create_new(ID,Pid). 

lookup_user(ID) ->
   tts_data:user_get_pid(ID). 

delete_user(ID) ->
    tts_data:user_delete(ID).

