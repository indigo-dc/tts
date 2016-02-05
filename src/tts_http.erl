% @doc the user webinterface
-module(tts_http).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("tts.hrl").
-record(state, {
          path = undefined,
          session = undefine,
          session_id = undefine,
          session_max_age = undefined,
          logged_in = false,
          header = undefined,
          qs = #{},
          body_qs = [],
          op_id = undefined
}).

-define(COOKIE,<<"tts_session">>).

init(_, Req, _Opts) ->
    extract_args(Req).   

handle(Req,#state{path=ep_redirect} = State) ->
    redirect_to_auth_server(Req, State);
handle(Req,#state{path=ep_return} = State) ->
    login_user_with_authcode(Req,State);
handle(Req,#state{path=ep_main} = State) ->
    redirect_to_user_page_or_login(Req, State);
handle(Req,#state{path=ep_user} = State) ->
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
    IdToken = oidcc:parse_and_validate_token(Token,OpenIdProviderId ,OidcNonce),
    ok = set_user(get_user(IdToken),Session),
    redirect_to(user_page, Req, State);
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
            show_html(Body, Req, State)
    end.

show_user_page(Req, State) ->
    {ok, Body} = tts_user_dtl:render([]), 
    show_html(Body, Req, State). 


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

show_html(Body, Req, State) ->
    {ok, Req2} = update_cookie_lifetime(Req,State),
    Req3 = cowboy_req:set_resp_body(Body,Req2),    
    {ok, Req4} = cowboy_req:reply(200, Req3),
    {ok, Req4, State}.


terminate(_Reason, _Req, _State) ->
	ok.


extract_args(Req) ->
    {Path,Req2} = cowboy_req:path(Req), 
    AtomPath = path_to_atom(Path),
    {QsMap, Req3} = extract_qs(Req2),
    {CookieSessionId,Req4} = cowboy_req:cookie(?COOKIE,Req3),
    {ok, Session} = tts_session_mgr:get_session(CookieSessionId),
    LoggedIn = tts_session:is_logged_in(Session),
    {ok, SessionId} = tts_session:get_id(Session),
    {ok, MaxAge} = tts_session:get_max_age(Session),
    {ok, BodyQs, Req5} = cowboy_req:body_qs(Req4), 
    State =#state{
              path = AtomPath,
              session = Session,
              session_id = SessionId,
              logged_in = LoggedIn,
              session_max_age = MaxAge,
              qs = QsMap,
              body_qs = BodyQs
             },
    {ok, Req5, State}.

update_cookie_lifetime(Req,#state{session_max_age = MaxAge, session_id = ID}) ->
    Opts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, ID, Opts, Req),
    {ok, Req2}.  

clear_cookie_and_close_session(Req,#state{session = Session}) ->
    Opts = [{max_age, 0}, {http_only, true}, {path, <<"/">>}],
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, Opts, Req),
    ok = tts_session:close(Session),
    {ok, Req2}.

path_to_atom(Path) ->
    EpRedirect = ?CONFIG(ep_redirect),
    EpReturn = ?CONFIG(ep_return),
    EpUser = ?CONFIG(ep_user),
    case Path of 
       EpRedirect -> ep_redirect;
       EpReturn -> ep_return;
       EpUser -> ep_user;
        _ -> ep_main
    end.


extract_qs(Req) ->
    {TupleList, Req2} = cowboy_req:qs_vals(Req),
    Map = create_map_from_proplist(TupleList),
    {Map, Req2}.

     
-define(KEYMAPPING,[
                    {<<"code">>, code},
                    {<<"error">>, error},
                    {<<"state">>, state}
                   ]).

create_map_from_proplist(List) ->
    KeyToAtom = fun({Key,Value},Map) ->
                        AtomKey = case lists:keyfind(Key,1,?KEYMAPPING) of
                                      {Key, AKey} -> AKey;
                                      false -> Key 
                                  end,
                        maps:put(AtomKey,Value,Map)
                end,
    lists:foldl(KeyToAtom,#{},List).


get_user({ok,Token}) ->
    tts_user:lookup_user(Token);
get_user(_) ->
    {error, invalid_token}.

set_user({ok, User},Session) ->
    tts_session:set_user(User, Session);
set_user(_,_Session) ->
    ok.

