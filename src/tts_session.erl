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
-export([get_userid/1]).

-export([get_max_age/1]).
-export([set_max_age/2]).

-export([set_token/2]).
-export([get_token/1]).

-export([get_user_info/1]).
-export([get_display_name/1]).

-export([set_iss_sub/3]).
-export([get_iss_sub/1]).

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

-spec start_link(ID :: binary()) -> {ok, pid()}.
start_link(ID) ->
    gen_server:start_link(?MODULE, ID, []).

-spec close(Pid :: pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

-spec get_id(Pid :: pid()) -> {ok, ID::binary()}.
get_id(undefined) ->
    {ok, undefined};
get_id(Pid) ->
    gen_server:call(Pid, get_id).

-spec get_userid(Pid :: pid()) -> {ok, ID::binary()}.
get_userid(Pid) ->
    gen_server:call(Pid, get_userid).

-spec get_max_age(Pid :: pid()) -> {ok, MaxAge::non_neg_integer()}.
get_max_age(Pid) ->
    gen_server:call(Pid, get_max_age).

-spec set_max_age(MaxAge :: pos_integer(), Pid :: pid()) -> ok.
set_max_age(MaxAge, Pid) ->
    gen_server:call(Pid, {set_max_age, MaxAge}).

-spec get_token(Pid :: pid()) -> {ok, Token::map()}.
get_token(Pid) ->
    gen_server:call(Pid, get_token).

-spec set_token(Token :: map(), Pid :: pid()) -> ok.
set_token(Token, Pid) ->
    gen_server:call(Pid, {set_token, Token}).

-spec get_user_info(Pid :: pid()) -> {ok, UserInfo::map()}.
get_user_info(Pid) ->
    gen_server:call(Pid, get_user_info).

-spec get_display_name(Pid :: pid()) -> {ok, Name::binary()}.
get_display_name(Pid) ->
    gen_server:call(Pid, get_display_name).

-spec get_iss_sub(Pid :: pid()) ->
    {ok, Issuer :: binary(), Subject :: binary()}.
get_iss_sub( Pid) ->
    gen_server:call(Pid, get_iss_sub).

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
          iss = undefined,
          sub = undefined,
          user_agent = undefined,
          ip = undefined,
          token = none,
          oidc_info = #{},
          max_age = 10
         }).


init(ID) ->
    lager:info("~p: session starting", [ID]),
    MaxAge = ?CONFIG(session_timeout, 10000),
    {ok, #state{id = ID, max_age=MaxAge}}.

handle_call(get_id, _From, #state{id=Id, max_age=MA}=State) ->
    {reply, {ok, Id}, State, MA};
handle_call(get_userid, _From, #state{max_age=MA}=State) ->
    {reply, userid(State), State, MA};
handle_call(get_max_age, _From, #state{max_age=MA}=State) ->
    {reply, {ok, MA}, State, MA};
handle_call({set_max_age, MA}, _From, State) ->
    {reply, ok, State#state{max_age=MA}, MA};
handle_call(get_iss_sub, _From,
            #state{max_age=MA, iss=Issuer, sub=Subject}=State) ->
    {reply, {ok, Issuer, Subject}, State, MA};
handle_call({set_iss_sub, Issuer, Subject}, _From, #state{max_age=MA}=State) ->
    {reply, ok, State#state{iss=Issuer, sub=Subject}, MA};
handle_call({set_token, Token0}, _From, #state{max_age=MA}=State) ->
    UserInfo0 = maps:get(user_info, Token0, #{}),
    IdToken = maps:get(id, Token0, #{}),
    NewState = case IdToken of
                   #{claims := #{iss := Issuer, sub := Subject } } ->
                       State#state{iss=Issuer, sub=Subject};
                   _ -> State
               end,
    RemoveClaims = [aud, at_hash, exp, azp, iat],
    Claims = maps:without(RemoveClaims, maps:get(claims, IdToken, #{})),
    UserInfo = maps:merge(UserInfo0, Claims),
    TokenKeys = [access, id, refresh],
    Token = maps:with(TokenKeys, Token0),
    {reply, ok, NewState#state{token=Token, oidc_info=UserInfo}, MA};
handle_call(get_token, _From, #state{max_age=MA, token=Token}=State) ->
    {reply, {ok, Token}, State, MA};
%% handle_call({set_user_info, UserInfo}, _From, #state{max_age=MA} = State) ->
%%     {reply, ok, State#state{user_info=UserInfo}, MA};
handle_call(get_user_info, _From,
            #state{max_age=MA, oidc_info=Info0, sub=Sub, iss=Iss} =State) ->
    {ok, UserId} = userid(State),
    Update = #{userid => UserId, iss => Iss, sub => Sub},
    Info = maps:merge(Info0, Update),
    {reply, {ok, Info}, State, MA};
handle_call(get_display_name, _From, #state{max_age=MA, sub=Subject, iss=Issuer,
                                            oidc_info=OidcInfo}=State) ->
    Name = case maps:get(name, OidcInfo, undefined) of
               undefined -> << Subject/binary, <<"@">>/binary, Issuer/binary >>;
               Other -> Other
           end,
    {reply, {ok, Name}, State, MA};
handle_call(is_logged_in, _From, #state{iss=Iss, sub=Sub, max_age=MA}=State)
  when is_binary(Iss), is_binary(Sub)->
    {reply, true, State, MA};
handle_call(is_logged_in, _From, #state{max_age=MA}=State) ->
    {reply, false, State, MA};
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
    lager:info("~p: session stopping", [ID]),
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{id=ID} = State) ->
    lager:info("~p: timeout, asking for termination", [ID]),
    tts_session_mgr:session_wants_to_close(ID),
    {noreply, State, 5000};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{id=ID}) ->
    lager:info("~p: session terminating", [ID]),
    tts_session_mgr:session_terminating(ID),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



userid(#state{iss=Issuer, sub=Subject})
  when is_binary(Issuer), is_binary(Subject) ->
    Id = base64url:encode(jsx:encode(#{issuer => Issuer, subject => Subject})),
    {ok, Id};
userid(_) ->
    {error, not_set}.
