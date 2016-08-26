-module(tts_user_cache).
%%
%% Copyright 2016 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").

-behaviour(gen_server).
-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([get_user_info/2]).
-export([get_user_info/3]).
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

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec get_user_info(Issuer :: binary(), Subject :: binary() ) ->
    {ok, UserInfo::tts:user_info()} | {error, term()}.
get_user_info(Issuer, Subject) ->
    get_user_info(Issuer, Subject, undefined).

-spec get_user_info(Issuer :: binary(), Subject :: binary() ,
                    AccessToken::binary() | undefined ) ->
    {ok, UserInfo::tts:user_info()} | {error, term()}.
get_user_info(Issuer, Subject, AccessToken) ->
    retrieve_userinfo_if_exists(Issuer, Subject, AccessToken).

-spec verify_cache() -> ok.
verify_cache() ->
    gen_server:cast(?MODULE, verify_cache_validity).

-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?MODULE, clear_cache).
%% gen_server.

-record(state, {
         }).


init([]) ->
    {ok, #state{}, 1}.

handle_call(clear_cache, _From, State) ->
    ok = clear_user_cache(),
    {reply, ok, State};
handle_call({insert, UserInfo}, _From, State) ->
    Result = sync_insert_new_user(UserInfo),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(verify_cache_validity, State) ->
    Interval = ?CONFIG(cache_check_interval),
    {ok, _} = timer:apply_after(Interval, ?MODULE, verify_cache, []),
    verify_cache_validity(),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    Interval = ?CONFIG(cache_check_interval),
    case Interval of
        undefined ->
            {noreply, State, 5000};
        _ ->
            {ok, _} = timer:apply_after(Interval, ?MODULE, verify_cache, []),
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



retrieve_userinfo_if_exists(Issuer, Subject, AccessToken) ->
    load_user_if_needed(lookup_user(Issuer, Subject), Issuer, Subject,
                        AccessToken).

load_user_if_needed({ok, UserInfo}, _, _, _) ->
    {ok, UserInfo};
load_user_if_needed({error, not_found}, Issuer, Subject, AccessToken) ->
    UserMap = #{type => openidconnect, subject => Subject, issuer => Issuer},
    IdhInfo = tts_idh:lookup_user(UserMap),
    case AccessToken of
        undefined ->
            FakeName = << Subject/binary, <<"@">>/binary, Issuer/binary >>,
            FakeInfo = #{sub => Subject, iss => Issuer, name => FakeName},
            insert_user(IdhInfo, {ok, FakeInfo}, Issuer);
        Token ->
            {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
            OidcInfo = oidcc:retrieve_user_info(Token, ProviderPid,
                                                Subject),
            insert_user(IdhInfo, OidcInfo, Issuer)
    end.

insert_user({ok, IdhInfo}, {ok, OidcInfo0}, Issuer) ->
    OidcInfo = maps:put(iss, Issuer, OidcInfo0),
    Info = #{site => IdhInfo, oidc => OidcInfo},
    gen_server:call(?MODULE, {insert, Info});
insert_user({error, Reason}, _, _) ->
    {error, Reason};
insert_user(_, {error, Reason}, _) ->
    {error, Reason};
insert_user(_, _, _) ->
    {error, unsupported_values}.

sync_insert_new_user(UserInfo) ->
    case add_new_user_entry(UserInfo) of
        ok -> {ok, UserInfo};
        {error, _} -> {error, {collision, UserInfo}}
    end.


%% functions with data access

clear_user_cache() ->
    ok = tts_data:user_clear_cache().

verify_cache_validity() ->
    Timeout = ?CONFIG(cache_timeout),
    {ok, _NumDel} = tts_data:user_delete_entries_older_than(Timeout).

add_new_user_entry(UserInfo) ->
    tts_data:user_insert_info(UserInfo, ?CONFIG(cache_max_entries)).

lookup_user(Issuer, Subject) ->
    tts_data:user_lookup_info(Issuer, Subject).


