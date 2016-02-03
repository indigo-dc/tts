% @doc the user webinterface
-module(tts_http).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("tts.hrl").
-record(state, {
}).

-define(COOKIE,<<"tts_session">>).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {Path,Req2} = cowboy_req:path(Req), 
    {ok, Session ,Req3} = get_session(Req2),
    handle_path(Path, Req3, Session, State).

handle_path(ep_redirect,Req, Session, State) -> 
    redirect_to_auth_server(Req, Session, State);
handle_path(ep_return,Req, Session, State) -> 
    login_user_with_authcode(Req, Session, State);
handle_path(ep_main,Req, Session, State) -> 
    show_user_page_or_login(tts_session:is_logged_in(Session), Req, Session, State);
handle_path(Path,Req, Session, State) -> 
    handle_path(path_to_atom(Path),Req, Session, State). 
    
path_to_atom(Path) ->
    EpRedirect = ?CONFIG(ep_redirect),
    EpReturn = ?CONFIG(ep_return),
    case Path of 
       EpRedirect -> ep_redirect;
       EpReturn -> ep_return;
        _ -> ep_main
    end.


show_user_page_or_login(true, Req, Session, State) ->
    show_user_page(Req, Session, State);
show_user_page_or_login(false, Req, Session, State) ->
    show_select_page(Req, Session, State).

show_select_page(Req, _Session, State) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    GetIdAndDesc = fun({Id, Pid}, List) ->
                           {ok,#{description := Desc}} =
                           oidcc:get_openid_provider_info(Pid),
                           [ [ Id, Desc ] | List]
                   end,
    OpList = lists:reverse(lists:foldl(GetIdAndDesc,[],OIDCList)),
    {ok, Body} = tts_ui_login_dtl:render([{oidc_op_list,OpList}]),
    Req2 = cowboy_req:set_resp_body(Body,Req),    
	{ok, Req3} = cowboy_req:reply(200, Req2),
	{ok, Req3, State}.


show_user_page(Req, _Session, State) ->
    cowboy_req:reply(302, [{<<"location">>, <<"/user">>}],Req),
    {ok, Req, State}.

redirect_to_auth_server(Req, Session, State) ->
    {ok, BodyQs, Req2} = cowboy_req:body_qs(Req), 
    {_, OpenIdProvider} = lists:keyfind(<<"id">>,1,BodyQs),
    ok = tts_session:set_oidc_provider(OpenIdProvider,Session),
    {ok, OidcState} = tts_session:get_oidc_state(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    {ok, Redirection} = oidcc:create_redirect_url(OpenIdProvider,OidcState, OidcNonce),
    cowboy_req:reply(302, [{<<"server">>, <<"Hiawatha">>},
                           {<<"location">>, Redirection}],Req2),
    {ok, Req2, State}.

get_session(Req) ->
    get_or_create_session(get_session_from_cookie(Req)).

get_session_from_cookie(Req) ->
    cowboy_req:cookie(?COOKIE,Req).

get_or_create_session({undefined, Req}) ->
    create_session(Req);
get_or_create_session({ID, Req}) ->
    create_session_on_error(lookup_session(ID),Req).

lookup_session(ID) ->
    tts_session_mgr:get_session(ID).

create_session_on_error({ok, Pid}, Req) ->
    update_cookie_lifetime(Pid,Req);
create_session_on_error(_, Req) ->
    create_session(Req).

create_session(Req) ->
    {ok, Pid} = tts_session_mgr:new_session(),
    update_cookie_lifetime(Pid, Req).

update_cookie_lifetime(Pid,Req) ->
    {ok, ID} = tts_session:get_id(Pid),
    {ok, MaxAge} = tts_session:get_max_age(Pid),
    Opts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, ID, Opts, Req),
    {ok, Pid, Req2}.  

clear_cookie_and_close_session(Session, Req) ->
    Opts = [ {http_only, true}, {path, <<"/">>}],
    CombOpts = [{max_age,0}|Opts],
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, CombOpts, Req),
    ok = tts_session:close(Session),
    {ok, Req2}.


login_user_with_authcode(Req, Session, State) ->
    {Map, Req2} = extract_qs(Req),
    decide_on_auth_code_reply(Map, Session, Req2, State).

decide_on_auth_code_reply(#{error := Error}, Session, Req, State) ->
    show_error_page(Error, Session, Req, State);
decide_on_auth_code_reply(#{code := AuthCode, state := ClientState}, Session,
                          Req, State) ->
    retreive_oidc_token_if_state_fits(tts_session:is_oidc_state(ClientState,Session),
                                     AuthCode, Session, Req, State).


retreive_oidc_token_if_state_fits(true, AuthCode, Session, Req, State) ->
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    handle_oidc_token(oidcc:retrieve_token(AuthCode, OpenIdProviderId), Session, Req,
                      State);
retreive_oidc_token_if_state_fits(false, _AuthCode, Session, Req, State) ->
    Desc = <<"the returned state did not fit this session">>,
    show_error_page(Desc, Session, Req, State).

handle_oidc_token({ok, Token},Session, Req, State) ->
    %TODO: get the OIDC token and extract user information and greet the user 
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    IdToken = oidcc:parse_and_validate_token(Token,OpenIdProviderId ,OidcNonce),
    Body = io_lib:format("the token are ~p",[IdToken]),
    Req2 = cowboy_req:set_resp_body(Body,Req),
    {ok, Req3} = cowboy_req:reply(200, Req2), 
    {ok, Req3, State};
handle_oidc_token({error, Error}, Session, Req, State) ->
    show_error_page(Error, Session, Req, State).


show_error_page(_ErrorDescription, Session,Req,State) ->
    {ok, Req2}=clear_cookie_and_close_session(Session, Req),
    %TODO: show some error page
	{ok, Req3} = cowboy_req:reply(400, Req2),
    {ok, Req3, State}.



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


terminate(_Reason, _Req, _State) ->
	ok.
