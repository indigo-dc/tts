-module(tts_temp_cred).
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
-export([add_cred/2]).
-export([get_cred/2]).
-export([exists/2]).
-export([stop/0]).


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

-spec add_cred(Credential :: map(), UserInfo::map()) -> {ok, Id::binary()}.
add_cred(Credential, UserInfo) ->
    gen_server:call(?MODULE, {add, Credential, UserInfo}).

-spec get_cred(CredId :: binary(), UserInfo::map()) -> {ok, Credential::map} |
                                                        {error, Reason::any()}.
get_cred(Id, UserInfo) ->
    gen_server:call(?MODULE, {get, Id, UserInfo}).

-spec exists(CredId :: binary(), UserInfo::map()) -> true | false.
exists(Id, UserInfo) ->
    gen_server:call(?MODULE, {exists, Id, UserInfo}).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call({add,  Cred, UserInfo}, _From, #state{creds = Creds} = State) ->
    UserId = get_userid(UserInfo),
    Id = gen_random_id(State),
    NewCreds = case UserId of
                   undefined -> Creds;
                   _ -> [{Id, UserId, Cred} | Creds]
               end,
    {reply, {ok, Id}, State#state{creds=NewCreds}};
handle_call({exists, Id, UserInfo}, _From, State) ->
    Result = credential_exists(Id, UserInfo, State),
    {reply, Result, State};
handle_call({get, Id, UserInfo}, _From, State) ->
    {Result, NewState} = get_credential(Id, UserInfo, true, State),
    {reply, Result, NewState};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_userid(#{site := #{uid := UserId}}) ->
    UserId;
get_userid(#{ uid := UserId}) ->
    UserId;
get_userid(_UserId) ->
    undefined.


gen_random_id(#state{creds = Creds} = State) ->
    Id = tts_utils:random_string(16),
    case lists:keyfind(Id, 1, Creds) of
        false -> Id;
        _ -> gen_random_id(State)
    end.

credential_exists(Id, UserInfo, State) ->
    case get_credential(Id, UserInfo, false, State) of
        {{ok, _}, State} ->
            true;
        {_, State} ->
            false
    end.

get_credential(Id, UserInfo, Delete, #state{creds = Creds} = State) ->
    UserId = get_userid(UserInfo),
    {Result, NewCreds} = case lists:keyfind(Id, 1, Creds) of
                             {Id, UserId, Cred} ->
                                 case Delete of
                                     true ->
                                         NC = lists:keydelete(Id, 1, Creds),
                                         {{ok, Cred}, NC};
                                     _ ->
                                         {{ok, Cred}, Creds}
                                 end;
                             _ ->
                                 {{error, not_found}, Creds}
                         end,
    {Result, State#state{creds = NewCreds}}.
