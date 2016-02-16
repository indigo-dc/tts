% @doc the user webinterface
-module(tts_http).

-export([handle/1]).
-include("tts.hrl").

handle(#{path := ep_redirect } = ReqMap) ->
    redirect_to_auth_server(ReqMap);
handle(#{path := ep_return} = ReqMap) ->
    login_user_with_authcode(ReqMap);
handle(#{path := ep_main} = ReqMap) ->
    redirect_to_user_page_or_login(ReqMap);
handle(#{path := ep_user, method := post} = ReqMap) ->
    handle_user_action(ReqMap);
handle(#{path := ep_user, method := get} = ReqMap) ->
    show_user_page_or_redirect(ReqMap).
    

redirect_to_auth_server(#{body_qs:=BodyQs} = ReqMap) ->
    case maps:get(id,BodyQs,undefined) of
        undefined -> 
            redirect_to(main, ReqMap);
        OpenIdProvider ->
            redirect_to(auth_server, maps:put(op_id ,OpenIdProvider,ReqMap))
    end.


login_user_with_authcode(#{qs := #{error := Error }} = ReqMap) ->
    show_error_page(Error, ReqMap);
login_user_with_authcode(#{qs := #{state:=OidcState}, session := Session} =
                         ReqMap) ->
    retreive_oidc_token_if_state_fits(tts_session:is_oidc_state(OidcState,Session),
                                     ReqMap).

retreive_oidc_token_if_state_fits(true, ReqMap) ->
    #{
       session := Session,
       qs := #{code := AuthCode } 
      } = ReqMap,
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    handle_oidc_token(oidcc:retrieve_token(AuthCode, OpenIdProviderId), ReqMap);
retreive_oidc_token_if_state_fits(false, ReqMap) ->
    Desc = <<"the returned state did not fit this session">>,
    show_error_page(Desc,  ReqMap).

handle_oidc_token({ok, Token}, #{session:=Session}=ReqMap) ->
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:clear_oidc_state_nonce(Session),
    VerToken = oidcc:parse_and_validate_token(Token,OpenIdProviderId ,OidcNonce),
    try_to_set_user(VerToken, ReqMap);
    %% redirect_to(user_page, Req, State);
handle_oidc_token({error, Error}, ReqMap) ->
    show_error_page(Error, ReqMap).



redirect_to_user_page_or_login(#{logged_in := true}=ReqMap) ->
    redirect_to(user_page, ReqMap);
redirect_to_user_page_or_login(ReqMap) ->
    show_select_page(ReqMap).

show_select_page(ReqMap) ->
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
            redirect_to(auth_server, maps:put(op_id,OpenIdProviderId,ReqMap));
        _ ->
            {ok, Body} = tts_ui_login_dtl:render([{oidc_op_list,OpList}]),
            #{body => Body, status => 200, cookie => update}
    end.


handle_user_action(#{body_qs:=#{action := logout}, session:=Session} = ReqMap) ->
    ok = tts_session:close(Session),
    maps:put(cookie, clear, redirect_to(ep_main,ReqMap));
handle_user_action(ReqMap) ->
    show_user_page(ReqMap).


show_user_page_or_redirect(#{session := Session}) ->
    show_user_page_or_redirect(tts_session:is_logged_in(Session),Session). 
show_user_page_or_redirect(true, Session) ->
    show_user_page(Session);
show_user_page_or_redirect(_, _) ->
    redirect_to(ep_main,#{}).

show_user_page(Session) ->
    {ok,User} = tts_session:get_user(Session),
    {ok, UserInfo} = tts_user:get_user_info(User),
    #{uid := Uid } = UserInfo,
    {ok, Credentials} = tts_user:get_credential_list(User),
    {ok, ServiceMapList} = tts_services:get_list_for_user(Uid),
    ServiceList = [ [Id, Type, Host, Desc] ||
                    #{id:=Id,type:=Type,host:=Host,description:=Desc} <- ServiceMapList],
    Params = [{username, Uid},
              {credential_list, Credentials},
              {service_list, ServiceList}
             ],
    {ok, Body} = tts_user_dtl:render(Params),
    #{body => Body, status => 200, cookie => update}.


try_to_set_user({ok, #{id := #{ sub := Subject, iss := Issuer}} = Token}, ReqMap) ->
    set_valid_user(tts_user_mgr:get_user(Subject,
                                         Issuer),maps:put(token,Token,ReqMap));
try_to_set_user(_, ReqMap) ->
    Error = <<"Invalid Token">>,
    show_error_page(Error, ReqMap).


set_valid_user({ok, UserPid},#{session := Session, token:=Token } = ReqMap) ->
    ok = tts_session:set_token(Token,Session),
    ok = tts_user:connect_session(Session,UserPid),
    ok = tts_session:set_user(UserPid, Session),
    redirect_to(user_page,ReqMap);
set_valid_user(_,ReqMap) ->
    Error = <<"Invalid/Unknown User">>,
    show_error_page(Error, ReqMap).

redirect_to(auth_server, #{op_id := OpenIdProvider, session:=Session}) ->
    {ok, OidcState} = tts_session:get_oidc_state(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:set_oidc_provider(OpenIdProvider,Session),
    {ok, Redirection} = oidcc:create_redirect_url(OpenIdProvider,OidcState, OidcNonce),
    create_redirection(Redirection);
redirect_to(user_page, _ReqMap) ->
    UserPath = ?CONFIG(ep_user),
    create_redirection(UserPath);
redirect_to(_, _ReqMap) ->
    create_redirection("/").

show_error_page(Error,#{session := Session}) ->
    ok = tts_session:close(Session),
    %TODO: show some error page
    #{error => Error, status => 400, cookie => clear}.

create_redirection(Url) ->
    #{header => [{<<"location">>,Url}], cookie => update, status => 302}.
     



