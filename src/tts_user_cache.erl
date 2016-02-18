-module(tts_user_cache).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_user_info/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_user_info(UserSuject :: binary(), Issuer :: binary()) -> {ok, UserInfo :: map()} | {error, term()}.
get_user_info(UserSubject, Issuer) ->
    retrieve_userinfo_if_exists(UserSubject, Issuer).


%% gen_server.
%% TODO: implement a cleaning preferably in tts_data or similar

-record(state, {
}).

-include("tts.hrl").

init([]) ->
	{ok, #state{}}.

handle_call({insert,UserInfo}, _From, State) ->
    Result = sync_insert_new_user(UserInfo),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(check_cache, State) ->
    verify_cache_validity(),
    {noreply,State};
handle_info(timeout, State) ->
    Interval = ?CONFIG(cache_check_interval),
    case Interval of 
        undefined ->
            {noreply,State,5000};
        _ ->
            timer:send_interval(Interval,?MODULE,check_cache),
            {noreply,State}
    end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



retrieve_userinfo_if_exists(Subject,Issuer) ->
    load_user_if_needed(lookup_user(Issuer, Subject), Subject, Issuer).
   
load_user_if_needed({ok,UserInfo},_, _) ->
    {ok,UserInfo};
load_user_if_needed({error, not_found}, Subject, Issuer) ->
    UserMap = #{sub => Subject, iss => Issuer},
    UserInfo = tts_idh:lookup_user(UserMap),
    insert_user(UserInfo).

insert_user({ok, NewUser}) ->
    {ok, UserInfo} = gen_server:call(?MODULE,{insert,NewUser}),
    {ok, UserInfo};
insert_user({error, Reason}) ->
    {error, Reason}.


sync_insert_new_user(UserInfo) ->
    ok = add_new_user_entry(UserInfo), 
    {ok, UserInfo}.


%% functions with data access

verify_cache_validity() ->
    tts_data:user_delete_entries_older_than(?CONFIG(cache_timeout)).

add_new_user_entry(UserInfo) ->
    ok = tts_data:user_insert_info(UserInfo). 

lookup_user(Issuer,Subject) ->
   tts_data:user_lookup_info(Issuer, Subject). 


