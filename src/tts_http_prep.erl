% @doc the user webinterface
-module(tts_http_prep).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("tts.hrl").
-record(state, {
          bad_request = false,
          req_map = #{}
}).

-define(COOKIE,<<"tts_session">>).

init(_, Req, _Opts) ->
    try extract_args(Req) of
        {ok, Req2, State} -> {ok, Req2, State}
    catch 
        _:_ -> {ok, Req, #state{bad_request=true}}
    end.   

handle(Req,#state{bad_request=true} = State) ->
    show_result(#{error => bad_request}, Req, State);
handle(Req,#state{req_map=ReqMap} = State) ->
    Result = tts_http:handle(ReqMap),
    show_result(Result,Req,State).


show_result(#{error := _Error} = Result,Req,#state{req_map=ReqMap} = State) ->
    Status = maps:get(status,Result, 400),
    Cookie = maps:get(cookie,Result,clear),
    Body = maps:get(body,Result,<<"">>),
    {ok, Req2} = perform_cookie_action(Cookie,Req,ReqMap),
    show_html(Body, Status, Req2, State);
show_result(#{body := Body} = Result,Req,#state{req_map=ReqMap} = State) ->
    Status = maps:get(status, Result, 200),
    Cookie = maps:get(cookie,Result,clear),
    {ok, Req2} = perform_cookie_action(Cookie,Req,ReqMap),
    show_html(Body, Status, Req2, State);
show_result(#{status := 302, header := Header} = Result,Req,#state{req_map=ReqMap} = State) ->
    Cookie = maps:get(cookie,Result,clear),
    {ok, Req2} = perform_cookie_action(Cookie,Req,ReqMap),
    create_redirection(Header, Req2, State).



create_redirection(Header, Req, State) ->
    {ok, Req2} = cowboy_req:reply(302, Header,Req),
    {ok, Req2, State}.

show_html(Body, Status, Req, State) ->
    Req2 = cowboy_req:set_resp_body(Body,Req),    
    {ok, Req3} = cowboy_req:reply(Status, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

-define(HTTPMETHODMAPPING, [
                        {<<"GET">>, get},
                        {<<"POST">>, post}
                       ]).

-define(PATHMAPPING, [
                        {?CONFIG(ep_redirect), ep_redirect},
                        {?CONFIG(ep_return), ep_return},
                        {?CONFIG(ep_user), ep_user}
                       ]).

-define(QSMAPPING,[
                    {<<"code">>, code},
                    {<<"error">>, error},
                    {<<"state">>, state},
                    {<<"action">>, action, value},
                    {<<"request">>, request},
                    {<<"revoke">>, revoke},
                    {<<"logout">>, logout},
                    {<<"id">>, id}
                   ]).

extract_args(Req) ->
    {Path,Req2} = cowboy_req:path(Req), 
    AtomPath = map_to_atom(Path,?PATHMAPPING,ep_main),
    {QsList, Req3} = cowboy_req:qs_vals(Req2),
    QsMap = create_map_from_proplist(QsList),
    {CookieSessionId,Req4} = cowboy_req:cookie(?COOKIE,Req3),
    {ok, Session} = tts_session_mgr:get_session(CookieSessionId),
    LoggedIn = tts_session:is_logged_in(Session),
    {ok, SessionId} = tts_session:get_id(Session),
    {ok, MaxAge} = tts_session:get_max_age(Session),
    {ok, BodyQsList, Req5} = cowboy_req:body_qs(Req4), 
    BodyQsMap = create_map_from_proplist(BodyQsList),
    PostAction = maps:get(action,BodyQsMap,undefined),
    {Method, Req6} = cowboy_req:method(Req5),
    AtomMethod = map_to_atom(Method, ?HTTPMETHODMAPPING), 
    ReqMap = #{
              path => AtomPath,
              method => AtomMethod,
              session => Session,
              session_id => SessionId,
              logged_in => LoggedIn,
              session_max_age => MaxAge,
              qs => QsMap,
              body_qs => BodyQsMap,
              post_action => PostAction
             },
    {ok, Req6, #state{req_map = ReqMap}}.

perform_cookie_action(clear,Req,_ReqMap) ->
    Opts = create_cookie_opts(0),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, Opts, Req),
    {ok, Req2};
perform_cookie_action(update,Req,#{session_max_age := MaxAge, session_id := ID}) ->
    Opts = create_cookie_opts(MaxAge),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, ID, Opts, Req),
    {ok, Req2}.  


create_cookie_opts(MaxAge) ->
    BasicOpts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    case ?CONFIG(ssl) of 
        true ->
            [{secure, true} | BasicOpts];
        _ ->
            BasicOpts
    end.

     

create_map_from_proplist(List) ->
    KeyToAtom = fun({Key,Value},Map) ->
                        {NewKey, NewVal} 
                        = case map_to_atom(Key, ?QSMAPPING) of
                              {val_too, AKey} -> 
                                  {AKey, map_to_atom(Value, ?QSMAPPING)};
                              AKey -> 
                                  {AKey, Value}
                          end,
                        maps:put(NewKey,NewVal,Map)
                end,
    lists:foldl(KeyToAtom,#{},List).


map_to_atom(Item, Mapping) ->
    map_to_atom(Item, Mapping, Item).

map_to_atom(Item, Mapping, Default) ->
    case lists:keyfind(Item,1,Mapping) of
        {Item, AItem} -> AItem;
        {Item, AItem, value} -> {val_too,AItem};
        {Item, AItem, _} -> AItem;
        false -> Default 
    end.
