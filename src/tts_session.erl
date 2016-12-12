-module(tts_session).
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
-export([start_link/1]).
-export([close/1]).

-export([get_id/1]).
-export([get_sess_token/1]).
-export([get_userid/1]).

-export([get_max_age/1]).
-export([set_max_age/2]).

-export([set_token/2]).

-export([set_error/2]).
-export([get_error/1]).

-export([get_user_info/1]).
-export([get_display_name/1]).

-export([set_iss_sub/3]).

-export([is_user_agent/2]).
-export([is_same_ip/2]).
-export([is_logged_in/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


%% API.

-spec start_link(Token :: binary()) -> {ok, pid()}.
start_link(Token) ->
    gen_server:start_link(?MODULE, [Token], []).

-spec close(Pid :: pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

-spec get_id(Pid :: pid()) -> {ok, ID::binary()}.
get_id(undefined) ->
    {ok, undefined};
get_id(Pid) ->
    gen_server:call(Pid, get_id).

-spec get_sess_token(Pid :: pid()) -> {ok, Token::binary()}.
get_sess_token(undefined) ->
    {ok, undefined};
get_sess_token(Pid) ->
    gen_server:call(Pid, get_sess_token).

-spec get_userid(Pid :: pid()) -> {ok, ID::binary()}.
get_userid(Pid) ->
    gen_server:call(Pid, get_userid).

-spec get_max_age(Pid :: pid()) -> {ok, MaxAge::non_neg_integer()}.
get_max_age(Pid) ->
    gen_server:call(Pid, get_max_age).

-spec set_max_age(MaxAge :: pos_integer(), Pid :: pid()) -> ok.
set_max_age(MaxAge, Pid) ->
    gen_server:call(Pid, {set_max_age, MaxAge}).

-spec set_token(Token :: map(), Pid :: pid()) -> ok.
set_token(Token, Pid) ->
    gen_server:call(Pid, {set_token, Token}).

-spec get_error(Pid :: pid()) -> {ok, Token::map()}.
get_error(Pid) ->
    gen_server:call(Pid, get_error).

-spec set_error(Error :: binary(), Pid :: pid()) -> ok.
set_error(Error, Pid) ->
    gen_server:call(Pid, {set_error, Error}).

-spec get_user_info(Pid :: pid()) -> {ok, UserInfo::map()}.
get_user_info(Pid) ->
    gen_server:call(Pid, get_user_info).

-spec get_display_name(Pid :: pid()) -> {ok, Name::binary()}.
get_display_name(Pid) ->
    gen_server:call(Pid, get_display_name).

-spec set_iss_sub(Issuer :: binary(), Subject::binary(), Pid :: pid()) -> ok.
set_iss_sub(Issuer, Subject, Pid) ->
    gen_server:call(Pid, {set_iss_sub, Issuer, Subject}).

-spec is_logged_in(Pid :: pid()) -> true | false.
is_logged_in(Pid) ->
    gen_server:call(Pid, is_logged_in).

is_user_agent(UserAgent, Pid) ->
    gen_server:call(Pid, {is_user_agent, UserAgent}).

is_same_ip(IP, Pid) ->
    gen_server:call(Pid, {is_same_ip, IP}).

%% gen_server.
-include("tts.hrl").
-record(state, {
          id = unkonwn,
          sess_token = undefined,
          user_agent = undefined,
          ip = undefined,
          error = <<"">>,
          user_info = undefined,
          max_age = 10
         }).


init([Token]) ->
    Id = self(),
    lager:info("SESS~p starting", [Id]),
    MaxAge = ?CONFIG(session_timeout, 10000),
    {ok, UserInfo} = tts_userinfo:new(),
    {ok, #state{id = Id, sess_token=Token, max_age=MaxAge, user_info=UserInfo}}.

handle_call(get_id, _From, #state{id=Id, max_age=MA}=State) ->
    {reply, {ok, Id}, State, MA};
handle_call(get_sess_token, _From, #state{sess_token=Token,
                                          max_age=MA}=State) ->
    {reply, {ok, Token}, State, MA};
handle_call(get_userid, _From, #state{max_age=MA, user_info=UserInfo}=State) ->
    Result = tts_userinfo:return(id, UserInfo),
    {reply, Result, State, MA};
handle_call(get_max_age, _From, #state{max_age=MA}=State) ->
    {reply, {ok, MA}, State, MA};
handle_call({set_max_age, MA}, _From, State) ->
    {reply, ok, State#state{max_age=MA}, MA};
handle_call({set_iss_sub, Issuer, Subject}, _From,
            #state{max_age=MA, user_info=Info}=State) ->
    {ok, NewInfo} = tts_userinfo:update_iss_sub(Issuer, Subject, Info),
    {reply, ok, State#state{user_info=NewInfo}, MA};
handle_call({set_token, Token}, _From,
            #state{user_info=Info, max_age=MA}=State) ->
    IdInfo = maps:get(user_info, Token, undefined),
    IdToken = maps:get(id, Token, undefined),
    AccToken = maps:get(access, Token, undefined),

    Info1 =
        case IdToken of
            undefined ->
                Info;
            _ ->
                {ok, Inf1} = tts_userinfo:update_id_token(IdToken, Info),
                Inf1
        end,
    Info2 =
        case IdInfo of
            undefined ->
                Info1;
            _ ->
                {ok, Inf2} = tts_userinfo:update_id_info(IdInfo, Info1),
                Inf2
        end,
    Info3 =
        case AccToken of
            undefined ->
                Info2;
            _ ->
                {ok, Inf3} = tts_userinfo:update_access_token(AccToken, Info2),
                Inf3
        end,
    {reply, ok, State#state{user_info=Info3}, MA};
handle_call({set_error, Error}, _From, #state{max_age=MA}=State) ->
    {reply, ok, State#state{error=Error}, MA};
handle_call(get_error, _From, #state{max_age=MA, error=Error}=State) ->
    {reply, {ok, Error}, State, MA};
handle_call(get_user_info, _From, #state{max_age=MA, user_info=Info} =State) ->
    {reply, {ok, Info}, State, MA};
handle_call(get_display_name, _Frm, #state{max_age=MA, user_info=Info}=State) ->
    Result = tts_userinfo:return(display_name, Info),
    {reply, Result, State, MA};
handle_call(is_logged_in, _From, #state{user_info=Info, max_age=MA}=State) ->
    Result = tts_userinfo:return(logged_in, Info),
    {reply, Result, State, MA};
handle_call({is_user_agent, UserAgent}, _From,
            #state{user_agent=undefined, max_age=MA}=State) ->
    {reply, true, State#state{user_agent=UserAgent}, MA};
handle_call({is_user_agent, UserAgent}, _From,
            #state{user_agent=UserAgent, max_age=MA}=State) ->
    {reply, true, State, MA};
handle_call({is_user_agent, _UserAgent}, _From, #state{max_age=MA}=State) ->
    {reply, false, State, MA};
handle_call({is_same_ip, IP}, _From,
            #state{ip=undefined, max_age=MA}=State) ->
    {reply, true, State#state{ip=IP}, MA};
handle_call({is_same_ip, IP}, _From,
            #state{ip=IP, max_age=MA}=State) ->
    {reply, true, State, MA};
handle_call({is_same_ip, _IP}, _From, #state{max_age=MA}=State) ->
    {reply, false, State, MA};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(close, #state{id=ID} = State) ->
    lager:debug("SESS~p stopping", [ID]),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{id=ID, sess_token=Token} = State) ->
    lager:info("SESS~p timeout, asking for termination", [ID]),
    tts_session_mgr:session_wants_to_close(Token),
    {noreply, State, 5000};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{id=ID, sess_token=Token}) ->
    lager:info("SESS~p terminating", [ID]),
    tts_session_mgr:session_terminating(Token),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
