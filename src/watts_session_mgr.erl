%% @doc This module runs as a single worker and serializes all needed session
%% creations to ensure uniqueness of tokens.
-module(watts_session_mgr).
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
-include("watts.hrl").
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).

-export([new_session/0]).
-export([get_session/1]).
-export([close_all_sessions/0]).
-export([session_wants_to_close/1]).
-export([session_terminating/1]).
-export([get_cookie_data/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
         }).
-type state() :: #state{}.

%% API.

%% @doc start link implementation of the gen_server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, noparams, []).

%% @doc send a cast to stop the process
-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).


%% @doc create a new session
-spec new_session() -> {ok, pid()}.
new_session() ->
    gen_server:call(?MODULE, new_session).


%% @doc get a session by the cookie data, just a simple ets lookup
-spec get_session(Cookie :: binary() | undefined) -> {ok, pid() | undefined}.
get_session(undefined) ->
    {ok, undefined};
get_session(Jwt) ->
    Key = ?CONFIG(jwt_key),
    case erljwt:validate(Jwt, [rs256], #{}, Key) of
        {ok, #{claims := #{ token := Token }}} ->
            lookup_session_pid(Token);
        _ ->
            {ok, undefined}
    end.


%% @doc get the max age and the content of the cookie for the session.
-spec get_cookie_data(Session :: pid())
                             -> {ok, MaxAge :: pos_integer(),
                                 CookieData :: binary()}.
get_cookie_data(Session) ->
    {ok, Max} = watts_session:get_max_age(Session),
    {ok, Token} = watts_session:get_sess_token(Session),
    Key = ?CONFIG(jwt_key),
    Jwt = erljwt:create(rs256, #{token => Token}, Max, Key),
    {ok, Max, Jwt}.


%% @doc a session wants to close, remove it and send a close back
-spec session_wants_to_close(Token :: binary()) -> ok.
session_wants_to_close(Token) ->
    gen_server:call(?MODULE, {close_session, Token}).


%% @doc a session died purge it
-spec session_terminating(ID :: any()) -> ok.
session_terminating(Token) ->
    gen_server:call(?MODULE, {purge_session, Token}).

%% @doc close all sessions, gracefully shutdown
-spec close_all_sessions() -> ok.
close_all_sessions() ->
    gen_server:call(?MODULE, close_all_sessions).

%% gen_server.

%% @doc init the session manager process
-spec init(noparams) -> {ok, state()}.
init(noparams) ->
    {ok, #state{}}.

%% @doc handle the calls coming in from the plublic functions
-spec handle_call(any(), any(), state()) -> {reply, atom() | tuple(), state()}.
handle_call(new_session, _From, State) ->
    Result = create_new_session(),
    {reply, Result, State};
handle_call({close_session, ID}, _From, State) ->
    ok = close_session(ID),
    {reply, ok, State};
handle_call({purge_session, ID}, _From, State) ->
    delete_session(ID),
    {reply, ok, State};
handle_call(close_all_sessions, _From, State) ->
    do_delete_all_sessions(),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc handle the stop cast form the stop function
-spec handle_cast(any(), state())
                 -> {stop, normal, state()} | {noreply, state()}.
handle_cast(stop, State) ->
    SessionList = get_all_sessions(),
    delete_sessions(SessionList),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc ignore all other messages
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc do nothing, just stop
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc does nothting
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc create a new session
-spec create_new_session() -> {ok, pid()}.
create_new_session() ->
    Token = get_unique_token(),
    Pid = start_session(Token),
    ok = set_token_for_session(Token, Pid),
    {ok, Pid}.

%% @doc delete and trigger a close at the session
-spec close_session(ID :: any()) -> ok.
close_session(ID) ->
    {ok, Pid} = lookup_session_pid(ID),
    delete_session(ID),
    watts_session:close(Pid),
    ok.

%% @doc delete all sessions
-spec do_delete_all_sessions() -> ok.
do_delete_all_sessions() ->
    SessionList = get_all_sessions(),
    delete_sessions(SessionList),
    ok.

%% @doc delete list of sessions
-spec delete_sessions([#{ id => Id :: any(), pid => pid()}]) -> ok.
delete_sessions([]) ->
    ok;
delete_sessions([#{id:= Id, pid:= Pid}|T]) ->
    watts_session:kill(Pid),
    delete_session(Id),
    delete_sessions(T).


%% @doc generate a unique token that can be stored in a cookie
%% @todo use signed JWTS as token
-spec get_unique_token() -> binary().
get_unique_token() ->
    Token = watts_utils:random_string(64),
    repeat_token_gen_if_needed(add_new_session_entry(Token)).

%% @doc start a session and set the token
-spec start_session(Token :: binary()) -> pid().
start_session(Token) ->
    {ok, Pid} = watts_session_sup:new_session(Token),
    Pid.

%% @doc repeat the generation of the token until it is unique
-spec repeat_token_gen_if_needed({ok, binary()} |
                                 {error, atom()}) -> binary().
repeat_token_gen_if_needed({ok, Token}) ->
    Token;
repeat_token_gen_if_needed({error, _}) ->
    get_unique_token().

%%
%% functions with data access
%%

%% @doc get all active sessions
-spec get_all_sessions() -> [#{ id => Id :: any(), pid => pid()}].
get_all_sessions() ->
    watts_ets:sessions_get_list().

%% @doc store a new session and return if succeeded
-spec add_new_session_entry(Token :: binary())
                           -> {ok, binary()} | {error, used}.
add_new_session_entry(Token) ->
    case watts_ets:sessions_create_new(Token) of
        ok -> {ok, Token};
        _ -> {error, used}
    end.

%% @doc lookup a session by its id
-spec lookup_session_pid(ID :: any() ) -> {ok, pid() | undefined}.
lookup_session_pid(ID) ->
    case watts_ets:sessions_get_pid(ID) of
        {ok, Pid} ->
            {ok, Pid};
        {error, _} ->
            {ok, undefined}
    end.

%% @doc set the token for the session
-spec set_token_for_session(Token :: binary(), Pid :: pid()) -> ok.
set_token_for_session(Token, Pid) ->
    watts_ets:sessions_update_pid(Token, Pid).

%% @doc delete the session with the given id
-spec delete_session(ID :: any()) -> ok.
delete_session(ID) ->
    watts_ets:sessions_delete(ID),
    ok.
