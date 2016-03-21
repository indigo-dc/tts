-module(tts_user_cache).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([get_user_info/2]).
-export([verify_cache/0]).
-export([clear_cache/0]).

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

-spec get_user_info(Issuer :: binary(), Subject :: binary() ) -> {ok, UserInfo :: map()} | {error, term()}.
get_user_info(Issuer, Subject) ->
    retrieve_userinfo_if_exists(Issuer, Subject).

-spec verify_cache() -> ok.
verify_cache() ->
    gen_server:cast(?MODULE,verify_cache_validity).

-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?MODULE,clear_cache).
%% gen_server.

-record(state, {
}).

-include("tts.hrl").

init([]) ->
	{ok, #state{}}.

handle_call(clear_cache, _From, State) ->
    ok = clear_user_cache(),
    {reply, ok, State};
handle_call({insert,UserInfo}, _From, State) ->
    Result = sync_insert_new_user(UserInfo),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(verify_cache_validity, State) ->
    Interval = ?CONFIG(cache_check_interval),
    {ok, _} = timer:apply_after(Interval,?MODULE,verify_cache,[]),
    verify_cache_validity(),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, State) ->
    Interval = ?CONFIG(cache_check_interval),
    case Interval of 
        undefined ->
            {noreply,State,5000};
        _ ->
            {ok, _} = timer:apply_after(Interval,?MODULE,verify_cache,[]),
            {noreply,State}
    end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



retrieve_userinfo_if_exists(Issuer,Subject) ->
    load_user_if_needed(lookup_user(Issuer, Subject),Issuer, Subject).
   
load_user_if_needed({ok,UserInfo},_, _) ->
    {ok,UserInfo};
load_user_if_needed({error, not_found}, Issuer, Subject) ->
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

clear_user_cache() ->
    tts_data:user_clear_cache().

verify_cache_validity() ->
    tts_data:user_delete_entries_older_than(?CONFIG(cache_timeout)).

add_new_user_entry(UserInfo) ->
    ok = tts_data:user_insert_info(UserInfo,?CONFIG(cache_max_entries)). 

lookup_user(Issuer,Subject) ->
   tts_data:user_lookup_info(Issuer, Subject). 


