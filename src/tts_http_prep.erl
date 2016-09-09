-module(tts_http_prep).
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

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("tts.hrl").
-record(state, {
          bad_request = true,
          config_loaded = undefined,
          req_map = #{}
         }).

init(_, Req, _Opts) ->
    try extract_args(Req, tts_config:is_loaded()) of
        {ok, Req2, State} -> {ok, Req2, State}
    catch
        _:_ -> {ok, Req, #state{}}
    end.

handle(Req, #state{config_loaded=false} = State) ->
    show_result(#{error => no_config}, Req, State);
handle(Req, #state{bad_request=true} = State) ->
    show_result(#{error => bad_request}, Req, State);
handle(Req, #state{req_map=ReqMap} = State) ->
    Result = tts_http:handle(ReqMap),
    show_result(Result, Req, State).


show_result(#{error := no_config}, Req, State) ->
    {ok, Version} = application:get_key(tts, vsn),
    {ok, Body} = tts_main_dtl:render([{version, Version}]),
    show_html(Body, 200, Req, State);
show_result(#{error := _Error} = Result, Req, #state{req_map=ReqMap} = State) ->
    Status = maps:get(status, Result, 400),
    Cookie = maps:get(cookie, Result, clear),
    Body = maps:get(body, Result, <<"">>),
    {ok, Req2} = perform_cookie_action(Cookie, Req, ReqMap),
    show_html(Body, Status, Req2, State);
show_result(#{body := Body} = Result, Req, #state{req_map=ReqMap} = State) ->
    Status = maps:get(status, Result, 200),
    Cookie = maps:get(cookie, Result, clear),
    {ok, Req2} = perform_cookie_action(Cookie, Req, ReqMap),
    show_html(Body, Status, Req2, State);
show_result(#{status := 302, header := Header} = Result, Req,
            #state{req_map=ReqMap} = State) ->
    Cookie = maps:get(cookie, Result, clear),
    {ok, Req2} = perform_cookie_action(Cookie, Req, ReqMap),
    {ok, Req3} = cowboy_req:reply(302, Header, Req2),
    {ok, Req3, State}.


show_html(Body, Status, Req, State) ->
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {ok, Req3} = cowboy_req:reply(Status, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

-define(HTTPMETHODMAPPING, [
                            {<<"GET">>, get},
                            {<<"POST">>, post}
                           ]).

-define(QSMAPPING, [
                   {<<"action">>, action, value},
                   {<<"request">>, request},
                   {<<"revoke">>, revoke},
                   {<<"logout">>, logout},
                   {<<"service_id">>, service_id},
                   {<<"credential_id">>, credential_id},
                   {<<"id">>, id}
                  ]).

extract_args(Req, false) ->
    {ok, Req, #state{ config_loaded = false }};
extract_args(Req, true) ->
    CookieName = tts_http_util:cookie_name(),
    {CookieSessionId, Req4} = cowboy_req:cookie(CookieName, Req),
    {ok, BodyQsList, Req5} = cowboy_req:body_qs(Req4),
    {Headers, Req6} = cowboy_req:headers(Req5),
    {Method, Req7} = cowboy_req:method(Req6),
    {{PeerIP, _Port}, Req99} = cowboy_req:peer(Req7),


    {ok, Session} = tts_session_mgr:get_session(CookieSessionId),
    {ok, SessionId} = tts_session:get_id(Session),
    AtomMethod = map_to_atom(Method, ?HTTPMETHODMAPPING),
    BodyQsMap = create_map_from_proplist(BodyQsList),
    UserAgent = get_header(<<"user-agent">>, Headers),
    LoggedIn = is_logged_in(UserAgent, PeerIP, Session),
    ReqMap = #{
      method => AtomMethod,
      session => Session,
      session_id => SessionId,
      logged_in => LoggedIn,
      body_qs => BodyQsMap
     },
    {ok, Req99, #state{req_map = ReqMap, config_loaded = true,
                       bad_request=false}}.

perform_cookie_action(clear, Req, _ReqMap) ->
    tts_http_util:perform_cookie_action(clear, 0, deleted, Req);
perform_cookie_action(update, Req, #{session := undefined}) ->
    perform_cookie_action(clear, Req, undefined);
perform_cookie_action(update, Req, #{session := Session}) ->
    {ok, MaxAge} = tts_session:get_max_age(Session),
    {ok, ID} = tts_session:get_id(Session),
    tts_http_util:perform_cookie_action(update, MaxAge, ID, Req).

create_map_from_proplist(List) ->
    KeyToAtom = fun({Key, Value}, Map) ->
                        {NewKey, NewVal} = map_to_atoms(Key, Value, ?QSMAPPING),
                        maps:put(NewKey, NewVal, Map)
                end,
    lists:foldl(KeyToAtom, #{}, List).

map_to_atoms(Key, Value, Mapping) ->
    case lists:keyfind(Key, 1, Mapping) of
        {Key, AKey, value} ->
            case lists:keyfind(Value, 1, Mapping) of
                {Value, AValue} ->
                    {AKey, AValue};
                _ ->
                    {AKey, Value}
            end;
        {Key, AKey} ->
            {AKey, Value};
        _ ->
            {Key, Value}
    end.

map_to_atom(Item, Mapping) ->
    map_to_atom(Item, Mapping, Item).

map_to_atom(Item, Mapping, Default) ->
    case lists:keyfind(Item, 1, Mapping) of
        {Item, AItem} -> AItem;
        {Item, AItem, _} -> AItem;
        false -> Default
    end.

is_logged_in(_UserAgent, _IP, undefined) ->
    false;
is_logged_in(UserAgent, IP, Session) ->
    LoggedIn = tts_session:is_logged_in(Session),
    SameUA = tts_session:is_user_agent(UserAgent, Session),
    SameIP = tts_session:is_same_ip(IP, Session),
    SameIP and SameUA and LoggedIn.

get_header(Key, Headers) ->
    case lists:keyfind(Key, 1, Headers) of
        {Key, Value} -> Value;
        false -> undefined
    end.
