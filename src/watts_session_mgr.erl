-module(tts_session_mgr).
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

%% API.
-export([start_link/0]).
-export([stop/0]).

-export([new_session/0]).
-export([get_session/1]).
-export([close_all_sessions/0]).
-export([session_wants_to_close/1]).
-export([session_terminating/1]).

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

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).

-spec new_session() -> {ok, pid()}.
new_session() ->
    gen_server:call(?MODULE, new_session).

-spec get_session(Token :: binary() | undefined) -> {ok, pid() | undefined}.
get_session(undefined) ->
    {ok, undefined};
get_session(Token) ->
    lookup_session_pid(Token).

-spec session_wants_to_close(Token :: binary()) -> ok.
session_wants_to_close(Token) ->
    gen_server:call(?MODULE, {delete_session, Token}).

-spec session_terminating(ID :: binary()) -> ok.
session_terminating(Token) ->
    gen_server:call(?MODULE, {purge_session, Token}).

-spec close_all_sessions() -> ok.
close_all_sessions() ->
    gen_server:call(?MODULE, close_all_sessions).

%% gen_server.

init([]) ->
    {ok, #state{}}.

handle_call(new_session, _From, State) ->
    Token = get_unique_token(),
    Pid = start_session(Token),
    ok = set_session_for_token(Token, Pid),
    {reply, {ok, Pid}, State};
handle_call({delete_session, ID}, _From, State) ->
    {ok, Pid} = lookup_session_pid(ID),
    delete_session(ID),
    tts_session:close(Pid),
    {reply, ok, State};
handle_call({purge_session, ID}, _From, State) ->
    delete_session(ID),
    {reply, ok, State};
handle_call(close_all_sessions, _From, State) ->
    SessionList = get_all_sessions(),
    delete_sessions(SessionList),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(stop, State) ->
    SessionList = get_all_sessions(),
    delete_sessions(SessionList),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


delete_sessions([]) ->
    ok;
delete_sessions([#{id:= Id, pid:= Pid}|T]) ->
    delete_session(Id),
    tts_session:close(Pid),
    delete_sessions(T).


get_unique_token() ->
    Token = tts_utils:random_string(64),
    repeat_id_gen_if_needed(add_new_session_entry(Token)).

start_session(Token) ->
    {ok, Pid} = watts_session_sup:new_session(Token),
    Pid.

repeat_id_gen_if_needed({ok, Token}) ->
    Token;
repeat_id_gen_if_needed(_) ->
    get_unique_token().

%%
%% functions with data access
%%
get_all_sessions() ->
    watts_data:sessions_get_list().

add_new_session_entry(Token) ->
    case watts_data:sessions_create_new(Token) of
        ok -> {ok, Token};
        _ -> {error, used}
    end.

lookup_session_pid(ID) ->
    case watts_data:sessions_get_pid(ID) of
        {ok, Pid} ->
            {ok, Pid};
        {error, _} ->
            {ok, undefined}
    end.

set_session_for_token(Token, Pid) ->
    watts_data:sessions_update_pid(Token, Pid).

delete_session(ID) ->
    watts_data:sessions_delete(ID),
    ok.
