% @doc the user webinterface
-module(tts_http).

-export([handle/1]).
-include("tts.hrl").

handle(#{path := ep_redirect } = ReqMap) ->
    redirect_to_auth_server(ReqMap);
handle(#{path := ep_return} = ReqMap) ->
    login_user_with_authcode(ReqMap);
handle(#{path := ep_main, logged_in := true} = ReqMap) ->
    redirect_to(user_page, ReqMap);
handle(#{path := ep_user, method := post, logged_in := true} = ReqMap) ->
    handle_user_action(ReqMap);
handle(#{path := ep_user, method := get, logged_in := true, session := Session}) ->
    show_user_page(Session);
handle(#{path := ep_main} = ReqMap) ->
    show_select_page(ReqMap);
handle( ReqMap) ->
    redirect_to(ep_main, ReqMap).
    

redirect_to_auth_server(#{body_qs:=BodyQs} = ReqMap) ->
    case maps:get(id,BodyQs,undefined) of
        undefined -> 
            redirect_to(main, ReqMap);
        OpenIdProvider ->
            redirect_to(auth_server, maps:put(op_id ,OpenIdProvider,ReqMap))
    end.


login_user_with_authcode(#{qs := #{error := Error }} = ReqMap) ->
    show_error_and_close_sessino(Error, ReqMap);
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
    show_error_and_close_sessino(Desc,  ReqMap).

handle_oidc_token({ok, Token}, #{session:=Session}=ReqMap) ->
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:clear_oidc_state_nonce(Session),
    VerToken = oidcc:parse_and_validate_token(Token,OpenIdProviderId ,OidcNonce),
    try_to_set_user(VerToken, ReqMap);
handle_oidc_token({error, Error}, ReqMap) ->
    show_error_and_close_sessino(Error, ReqMap).


show_select_page(ReqMap) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    GetIdAndDesc = fun({Id, Pid}, List) ->
                           {ok,#{description := Desc}} =
                           oidcc:get_openid_provider_info(Pid),
                           [ [ Id, Desc ] | List]
                   end,
    OpList = lists:reverse(lists:foldl(GetIdAndDesc,[],OIDCList)),
    redirect_to_op_or_show_select_page(OpList,ReqMap).

redirect_to_op_or_show_select_page([[OpenIdProviderId,_]],ReqMap) ->
    redirect_to(auth_server, maps:put(op_id,OpenIdProviderId,ReqMap));
redirect_to_op_or_show_select_page(OpList,_) ->
    {ok, Body} = tts_ui_login_dtl:render([{oidc_op_list,OpList}]),
    #{body => Body, status => 200, cookie => update}.


handle_user_action(#{body_qs:=#{action := request}} = ReqMap) ->
   request_credential(ReqMap); 
handle_user_action(#{body_qs:=#{action := revoke}} = ReqMap) ->
   revoke_credential(ReqMap); 
handle_user_action(#{body_qs:=#{action := logout}, session:=Session} = ReqMap) ->
    ok = tts_session:close(Session),
    maps:put(cookie, clear, redirect_to(ep_main,ReqMap));
handle_user_action(ReqMap) ->
    show_user_page(ReqMap).


request_credential(#{session := Session, body_qs:= #{ service_id:=ServiceId}}) ->
    {ok,Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok,UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    {ok,Token} = tts_session:get_token(Session),
    tts_service:request_credential(ServiceId,UserInfo,Token,[]),
    show_user_page(Session);
request_credential(ReqMap) ->
    Desc = <<"">>,
    show_error(Desc,ReqMap,false).

revoke_credential(#{session := Session}) ->
    show_user_page(Session).

show_user_page(Session) ->
    {ok,Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok,UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    UserId = maps:get(uid,UserInfo),
    {ok, Credentials} = tts_service:get_credential_list(UserId),
    {ok, ServiceMapList} = tts_service:get_service_list(UserId),
    ServiceList = [ [Id, Type, Host, Desc] ||
                    #{id:=Id,type:=Type,host:=Host,description:=Desc} <- ServiceMapList],
    Params = [{username, UserId},
              {credential_list, Credentials},
              {service_list, ServiceList}
             ],
    {ok, Body} = tts_user_dtl:render(Params),
    #{body => Body, status => 200, cookie => update}.


try_to_set_user({ok, #{id := #{ sub := Subject, iss := Issuer}} = Token}, ReqMap) ->
    set_valid_user(tts_user_cache:get_user_info(Issuer, Subject), maps:put(token,Token,ReqMap));
try_to_set_user(_, ReqMap) ->
    Error = <<"Invalid Token">>,
    show_error_and_close_sessino(Error, ReqMap).


set_valid_user({ok, _UserInfo},#{session := Session, token:=Token } = ReqMap) ->
    ok = tts_session:set_token(Token,Session),
    redirect_to(user_page,ReqMap);
set_valid_user(_,ReqMap) ->
    Error = <<"Invalid/Unknown User">>,
    show_error_and_close_sessino(Error, ReqMap).

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

show_error_and_close_sessino(Error,ReqMap) ->
    show_error(Error, ReqMap, true).

show_error(Error,#{session := Session},CloseSession) ->
    CookieAction = case CloseSession of
                       true ->
                           ok = tts_session:close(Session),
                           clear;
                       false ->
                           update
                   end,
    #{error => Error, status => 400, cookie => CookieAction}.


create_redirection(Url) ->
    #{header => [{<<"location">>,Url}], cookie => update, status => 302}.
     

