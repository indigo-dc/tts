% @doc the user webinterface
-module(tts_http).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("tts.hrl").
-record(state, {
          path = undefined,
          method = undefined,
          session = undefine,
          session_id = undefine,
          session_max_age = undefined,
          logged_in = false,
          header = undefined,
          qs = #{},
          body_qs = #{},
          token = #{},
          op_id = undefined,
          bad_request = false
}).

-define(COOKIE,<<"tts_session">>).

init(_, Req, _Opts) ->
    try extract_args(Req) of
        {ok, Req2, State} -> {ok, Req2, State}
    catch 
        _:_ -> {ok, Req, #state{bad_request=true}}
    end.   

handle(Req,#state{bad_request=true} = State) ->
    show_error_page(bad_request, Req, State);
handle(Req,#state{path=ep_redirect} = State) ->
    redirect_to_auth_server(Req, State);
handle(Req,#state{path=ep_return} = State) ->
    login_user_with_authcode(Req,State);
handle(Req,#state{path=ep_main} = State) ->
    redirect_to_user_page_or_login(Req, State);
handle(Req,#state{path=ep_user, method=post} = State) ->
    handle_user_action(Req,State);
handle(Req,#state{path=ep_user, method=get} = State) ->
    show_user_page(Req, State).
    

redirect_to_auth_server(Req, #state{body_qs=BodyQs} = State) ->
    case lists:keyfind(<<"id">>,1,BodyQs) of
        {_, OpenIdProvider} ->
            redirect_to(auth_server, Req, State#state{op_id = OpenIdProvider});
        _ -> 
            redirect_to(main, Req, State)
    end.


login_user_with_authcode(Req, #state{qs = #{error := Error }} = State) ->
    show_error_page(Error, Req, State);
login_user_with_authcode(Req, #state{qs = #{state:=OidcState}, session = Session} = State) ->
    retreive_oidc_token_if_state_fits(tts_session:is_oidc_state(OidcState,Session), Req, State).

retreive_oidc_token_if_state_fits(true, Req, State) ->
    #state{
       session = Session,
       qs = #{code := AuthCode } 
      } = State,
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    handle_oidc_token(oidcc:retrieve_token(AuthCode, OpenIdProviderId), Req,
                      State);
retreive_oidc_token_if_state_fits(false, Req, State) ->
    Desc = <<"the returned state did not fit this session">>,
    show_error_page(Desc,  Req, State).

handle_oidc_token({ok, Token}, Req, #state{session=Session}=State) ->
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:clear_oidc_state_nonce(Session),
    VerToken = oidcc:parse_and_validate_token(Token,OpenIdProviderId ,OidcNonce),
    try_to_set_user(VerToken, Req,State);
    %% redirect_to(user_page, Req, State);
handle_oidc_token({error, Error}, Req, State) ->
    show_error_page(Error, Req, State).



redirect_to_user_page_or_login(Req, #state{logged_in = true}=State) ->
    redirect_to(user_page, Req, State);
redirect_to_user_page_or_login(Req, State) ->
    show_select_page(Req, State).

show_select_page(Req, State) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    GetIdAndDesc = fun({Id, Pid}, List) ->
                           {ok,#{description := Desc}} =
                           oidcc:get_openid_provider_info(Pid),
                           [ [ Id, Desc ] | List]
                   end,
    OpList = lists:reverse(lists:foldl(GetIdAndDesc,[],OIDCList)),
    case length(OpList) of 
        1 ->
            [[OpenIdProviderId,_]] = OpList,
            redirect_to(auth_server, Req, State#state{op_id = OpenIdProviderId});
        _ ->
            {ok, Body} = tts_ui_login_dtl:render([{oidc_op_list,OpList}]),
            show_html_and_update_cookie(Body, Req, State)
    end.


handle_user_action(Req, #state{body_qs=#{action := logout}, session=Session} = State) ->
    {ok, Req2}=clear_cookie_and_close_session(Session, Req),
    redirect_to(ep_main,Req2,State);
handle_user_action(Req, State) ->
    show_user_page(Req,State).


show_user_page(Req, #state{session=Session}=State) ->
    {ok,User} = tts_session:get_user(Session),
    {ok, UserInfo} = tts_user:get_user_info(User),
    #{username := UserName } = UserInfo,
    {ok, Credentials} = tts_user:get_credential_list(User),
    Params = [{username, UserName},
              {credential_list, Credentials}],
    {ok, Body} = tts_user_dtl:render(Params), 
    show_html_and_update_cookie(Body, Req, State). 


try_to_set_user({ok, #{id := #{ sub := Subject, iss := Issuer}} = Token}, Req, State) ->
    set_valid_user(tts_user_mgr:get_user(Subject, Issuer), Req,
                   State#state{token = Token});
try_to_set_user(_, Req, State) ->
    Error = <<"Invalid Token">>,
    show_error_page(Error, Req, State).


set_valid_user({ok, Pid},Req,#state{session = Session, token=Token } = State) ->
    ok = tts_user:add_token(Token,Pid),
    ok = tts_user:connect_session(Session,Pid),
    ok = tts_session:set_user(Pid, Session),
    redirect_to(user_page,Req,State);
set_valid_user(_,Req,State) ->
    Error = <<"Invalid/Unknown User">>,
    show_error_page(Error, Req, State).

redirect_to(auth_server, Req, #state{op_id = OpenIdProvider, session=Session} = State) ->
    {ok, OidcState} = tts_session:get_oidc_state(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:set_oidc_provider(OpenIdProvider,Session),
    {ok, Redirection} = oidcc:create_redirect_url(OpenIdProvider,OidcState, OidcNonce),
    create_redirection(Redirection,Req,State);
redirect_to(user_page, Req, State) ->
    UserPath = ?CONFIG(ep_user),
    create_redirection(UserPath,Req,State);
redirect_to(_, Req, State) ->
    create_redirection("/",Req,State).

show_error_page(_ErrorDescription,Req,#state{session = Session} = State) ->
    {ok, Req2}=clear_cookie_and_close_session(Session, Req),
    %TODO: show some error page
	{ok, Req3} = cowboy_req:reply(400, Req2),
    {ok, Req3, State}.

create_redirection(Url, Req, State) ->
    {ok, Req2} = update_cookie_lifetime(Req,State),
    {ok, Req3} = cowboy_req:reply(302, [{<<"location">>, Url}],Req2),
    {ok, Req3, State}.

show_html_and_update_cookie(Body, Req, State) ->
    {ok, Req2} = update_cookie_lifetime(Req,State),
    only_show_html(Body, Req2, State).

only_show_html(Body, Req, State) ->
    Req2 = cowboy_req:set_resp_body(Body,Req),    
    {ok, Req3} = cowboy_req:reply(200, Req2),
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
    {Method, Req6} = cowboy_req:method(Req5),
    AtomMethod = map_to_atom(Method, ?HTTPMETHODMAPPING), 
    State =#state{
              path = AtomPath,
              method = AtomMethod,
              session = Session,
              session_id = SessionId,
              logged_in = LoggedIn,
              session_max_age = MaxAge,
              qs = QsMap,
              body_qs = BodyQsMap
             },
    {ok, Req6, State}.

update_cookie_lifetime(Req,#state{session_max_age = MaxAge, session_id = ID}) ->
    Opts = create_cookie_opts(MaxAge),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, ID, Opts, Req),
    {ok, Req2}.  

clear_cookie_and_close_session(Req,#state{session = Session}) ->
    Opts = create_cookie_opts(0),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, Opts, Req),
    ok = tts_session:close(Session),
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
