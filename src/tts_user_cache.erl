-module(tts_user_cache).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_user_id/2]).
-export([get_user_info/1]).

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

-spec get_user_id(UserSuject :: binary(), Issuer :: binary()) -> {ok, binary()} | {error, term()}.
get_user_id(UserSubject, Issuer) ->
    return_user_if_exists(UserSubject, Issuer).

-spec get_user_info(UserId :: binary() ) -> {ok, UserInfo :: map()} | {error, term()}.
get_user_info(UserId) ->
    lookup_user(UserId).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({insert,UserInfo}, _From, State) ->
    Result = atomic_insert_new_user(UserInfo),
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



return_user_if_exists(Subject,Issuer) ->
    load_user_if_needed(lookup_user({Issuer, Subject}), Subject, Issuer).
   
load_user_if_needed({ok,UserId},_, _) ->
    {ok, UserId};
load_user_if_needed({error, not_found}, Subject, Issuer) ->
    UserMap = #{sub => Subject, iss => Issuer},
    UserInfo = tts_idh:lookup_user(UserMap),
    insert_user(UserInfo).

insert_user({ok, UserInfo}) ->
    {ok, UserPid} = gen_server:call(?MODULE,{insert,UserInfo}),
    ok = tts_user:set_user_info(UserInfo,UserPid),
    {ok, UserPid};
insert_user({error, not_found}) ->
    {error, not_found}.



atomic_insert_new_user(UserInfo) ->
    UserIdList = maps:get(user_ids,UserInfo,[]),
    [UserId |_] = UserIdList,
    case add_new_user_entries(UserIdList,UserId,UserInfo) of
        ok -> 
            {ok, UserId};
        {error, already_exists} ->
            lookup_user(UserId)
    end.



add_new_user_entries(Mappings,UserId, UserInfo) ->
    Entries = [{Mapping,UserId} || Mapping <- Mappings], 
    tts_data:user_insert_mappings([{UserId,UserInfo}|Entries]). 

lookup_user(IssSub) ->
   tts_data:user_get_pid(IssSub). 

%% delete_user_mappings(IssSubList) ->
%%     tts_data:user_delete_mappings(IssSubList).

