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
    load_and_return_user(UserSubject, Issuer).

-spec user_wants_to_shutdown(Pid :: pid()) -> ok.
user_wants_to_shutdown(Pid) ->
    gen_server:cast(?MODULE,{user_wants_to_shutdown,Pid}).

%% gen_server.

init([]) ->
	{ok, #state{}}.

handle_call({create,UserIdList}, _From, State) ->
    Result = create_new_user(UserIdList),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({user_wants_to_shutdown,Pid}, State) ->
    {ok, UserInfo} = tts_user:get_user_info(Pid),
    #{user_ids := UserIDs} = UserInfo,
    true = delete_user_mappings(UserIDs),
    ok = tts_user:shutdown(Pid),
    {noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



load_and_return_user(Subject,Issuer) ->
    load_user_if_needed(lookup_user({Subject,Issuer}), Subject, Issuer).
   
load_user_if_needed({ok,Pid},_, _) ->
    {ok, Pid};
load_user_if_needed({error, not_found}, Subject, Issuer) ->
    UserMap = #{sub => Subject, iss => Issuer},
    UserInfo = tts_idh:lookup_user(UserMap),
    create_user({Subject, Issuer}, UserInfo).

create_user(UserId, {ok, UserInfo}) ->
    % TODO: maybe add a way to add multiple representations
    UserIds = maps:get(user_ids,UserInfo,[]),
    NewUserIds = [UserId | UserIds],
    NewUserInfo = maps:put(user_ids,NewUserIds,UserInfo),
    {ok, UserPid} = gen_server:call(?MODULE,{create,NewUserIds}),
    ok = tts_user:set_user_info(NewUserInfo,UserPid),
    {ok, UserPid};
create_user(_, {error, not_found}) ->
    {error, not_found}.



create_new_user(UserIdList) ->
    {ok, Pid} = tts_user_sup:add_user(),
    [UserId |_] = UserIdList,
    case add_new_user_entries(UserIdList,Pid) of
        ok -> 
            {ok, Pid};
        {error, already_exists} ->
            ok = tts_user:stop(Pid),
            lookup_user(UserId)
    end.



%% 
%% functions with data access
%%

%% add_new_user_entry(ID,Pid) ->
%%    tts_data:user_create_new(ID,Pid). 
%%
add_new_user_entries(IDs,Pid) ->
    Entries = [{ID,Pid} || ID <- IDs], 
    tts_data:user_insert_mappings(Entries). 

lookup_user(ID) ->
   tts_data:user_get_pid(ID). 

delete_user_mappings(IDs) ->
    tts_data:user_delete_mappings(IDs).

