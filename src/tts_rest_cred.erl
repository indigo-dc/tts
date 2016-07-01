-module(tts_rest_cred).
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

-spec add_cred(Credential :: map(), UserId::binary()) -> {ok, Id::binary()}.
add_cred(Credential, UserId) ->
    gen_server:call(?MODULE, {add, Credential, UserId}).

-spec get_cred(CredId :: binary(), UserId::binary()) -> {ok, Credential::map} |
                                                        {error, Reason::any()}.
get_cred(Id, UserId) ->
    gen_server:call(?MODULE, {get, Id, UserId}).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

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


gen_random_id(#state{creds = Creds} = State) ->
    Id = tts_utils:random_string(16),
    case lists:keyfind(Id, 1, Creds) of
        false -> Id;
        _ -> gen_random_id(State)
    end.
