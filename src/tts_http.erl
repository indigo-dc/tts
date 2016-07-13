-module(tts_http).
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
handle(#{path := ep_user, method := get, logged_in := true,
         session := Session}) ->
    show_user_page(Session);
handle(#{path := ep_main} = ReqMap) ->
    show_select_page(ReqMap);
handle( ReqMap) ->
    redirect_to(ep_main, ReqMap).


redirect_to_auth_server(#{body_qs:=BodyQs, session_id:=SessionId} = ReqMap) ->
    case maps:get(id, BodyQs, undefined) of
        undefined ->
            lager:info("~p: trying to redirect without provider", [SessionId]),
            redirect_to(main, ReqMap);
        OpenIdProvider ->
            lager:debug("~p: redirecting to provider ~p", [SessionId,
                                                         OpenIdProvider]),
            redirect_to(auth_server, maps:put(op_id , OpenIdProvider, ReqMap))
    end.


login_user_with_authcode(#{qs := #{error := Error }
                           , session_id:=SessionId} = ReqMap) ->
    lager:warning("~p: error with auth code ~p~n", [SessionId, Error]),
    show_error_and_close_session(Error, ReqMap);
login_user_with_authcode(#{qs := #{state:=OidcState}, session := Session} =
                         ReqMap) ->
    IsOidc = tts_session:is_oidc_state(OidcState, Session),
    retreive_oidc_token_if_state_fits(IsOidc, ReqMap);
login_user_with_authcode(#{session_id := SessionId } = ReqMap) ->
    lager:error("~p: bad auth code response ~p~n", [SessionId, ReqMap]),
    Desc = <<"the data provided by the openid provider was invalide, please try
             again">>,
    show_error_and_close_session(Desc, ReqMap).


retreive_oidc_token_if_state_fits(true, ReqMap) ->
    #{
                                          session := Session,
                                          qs := #{code := AuthCode }
                                         } = ReqMap,
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    handle_oidc_token(oidcc:retrieve_token(AuthCode, OpenIdProviderId), ReqMap);
retreive_oidc_token_if_state_fits(false, #{session_id := SessionId} = ReqMap) ->
    lager:info("~p: wrong state in jwt ~n", [SessionId]),
    Desc = <<"the returned state did not fit this session">>,
    show_error_and_close_session(Desc, ReqMap).

handle_oidc_token({ok, Token}, #{session:=Session}=ReqMap) ->
    {ok, OpenIdProviderId} = tts_session:get_oidc_provider(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:clear_oidc_state_nonce(Session),
    VerToken = oidcc:parse_and_validate_token(Token, OpenIdProviderId,
                                              OidcNonce),
    try_to_set_user(VerToken, ReqMap);
handle_oidc_token({error, Error}, #{session_id := SessionId} = ReqMap) ->
    lager:warning("~p: bad token ~p", [SessionId, Error]),
    Desc = <<"An error occured on verifying your identity, please try to log in
             again">>,
    show_error_and_close_session(Desc, ReqMap).


show_select_page(ReqMap) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    GetIdAndDesc = fun({Id, Pid}, List) ->
                           {ok, #{description := Desc,
                                  ready := Enabled }} =
                           oidcc:get_openid_provider_info(Pid),
                           [ [ Id, Desc, Enabled ] | List]
                   end,
    OpList = lists:reverse(lists:foldl(GetIdAndDesc, [], OIDCList)),
    {ok, Version} = application:get_key(tts, vsn),
    {ok, Body} = tts_main_dtl:render([{ep_redirect, ?CONFIG(ep_redirect)},
                                      {oidc_op_list, OpList},
                                      {version, Version},
                                      {configured, true}]),
    Cookie = maps:get(cookie, ReqMap, update),
    #{body => Body, status => 200, cookie => Cookie}.


handle_user_action(#{body_qs:=#{action := request}} = ReqMap) ->
    request_credential(ReqMap);
handle_user_action(#{body_qs:=#{action := revoke}} = ReqMap) ->
    revoke_credential(ReqMap);
handle_user_action(#{body_qs:=#{action := logout}, session:=Session}
                   = ReqMap) ->
    ok = tts_session:close(Session),
    maps:put(cookie, clear, redirect_to(ep_main, ReqMap));
handle_user_action(ReqMap) ->
    show_user_page(ReqMap).


request_credential(#{session := Session, session_id := SessionId,
                     body_qs:= #{ service_id:=ServiceId}}) ->
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok, UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    {ok, Token} = tts_session:get_token(Session),
    true = tts_service:is_enabled(ServiceId),
    Iface = <<"web interface">>,
    case  tts_credential:request(ServiceId, UserInfo, Iface, Token, []) of
        {ok, Credential, Log} ->
            [#{name := id, value:=CredId}|_] = Credential,
            lager:info("~p: requested credential ~p",
                       [SessionId, CredId]),
            show_user_page(Session, Credential, Log);
        {error, Reason, _Log} ->
            lager:warning("~p: credential request for ~p failed with ~p",
                       [SessionId, ServiceId, Reason]),
            show_user_page(Session, <<"failed to request credential">>)
    end;
request_credential(#{session := Session}) ->
    Desc = <<"Credential Request failed">>,
    show_user_page(Session, Desc).


revoke_credential(#{session := Session, session_id:=SessionId,
                    body_qs:= #{ credential_id:=CredId}}) ->
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok, UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    case tts_credential:revoke(CredId, UserInfo) of
        {ok, _Result, _Log} ->
            lager:info("~p: revoked credential ~p as ~p ~p",
                       [SessionId, CredId, Issuer, Subject]),
            show_user_page(Session);
        {error, Error, _Log} ->
            lager:warning("~p: revocation of credential ~p  as ~p ~p
            failed with ~p", [SessionId, CredId, Issuer, Subject, Error]),
            show_user_page(Session, Error)
    end.

show_user_page(Session) ->
    show_user_page(Session, false, [], false).

show_user_page(Session, Error) ->
    show_user_page(Session, false, [], Error).

show_user_page(Session, Credential, Log) ->
    show_user_page(Session, Credential, Log, false).

show_user_page(Session, Credential, Log, Error) ->
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok, UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    #{ site := #{uid := UserId}} = UserInfo,
    {ok, ServiceList} = tts_service:get_list(UserId),
    {ok, CredentialList} = tts_credential:get_list(UserId),
    {ok, Version} = application:get_key(tts, vsn),
    {ok, #{access := #{token := AccessToken}}} = tts_session:get_token(Session),
    {ok, Name} = tts_session:get_display_name(Session),
    BaseParams = [
                  {error, Error},
                  {name, Name},
                  {credential, Credential},
                  {credential_log, Log},
                  {service_list, ServiceList},
                  {credential_list, CredentialList},
                  {access_token, AccessToken},
                  {logged_in, true},
                  {version, Version}
                 ],
    Params = case ?DEBUG_MODE of
                 true ->
                     {ok, Token} = tts_session:get_token(Session),
                     TokenText = io_lib:format("~p", [Token]),
                     [{token, TokenText} |BaseParams];
                 _ -> BaseParams
             end,
    {ok, Body} = tts_main_dtl:render(Params),
    #{body => Body, status => 200, cookie => update}.


try_to_set_user({ok, #{id := #{ sub := Subject, iss := Issuer},
                      access := #{token := AccessToken}} = Token},
                ReqMap) ->
    set_valid_user(tts_user_cache:get_user_info(Issuer, Subject, AccessToken),
                   maps:put(token, Token, ReqMap));
try_to_set_user(_, ReqMap) ->
    Error = <<"Invalid Token">>,
    show_error_and_close_session(Error, ReqMap).


set_valid_user({ok, UserInfo}, ReqMap) ->
    #{session := Session, session_id:=SessionId, token:=Token } = ReqMap,
    #{ oidc := #{ name := Name,
                  sub := Subject,
                  iss := Issuer }} = UserInfo,
    ok = tts_session:set_token(Token, Session),
    ok = tts_session:set_user_info(UserInfo, Session),
    lager:info("~p: user logged in ~p: ~p ~p",
               [SessionId, Name, Issuer, Subject]),
    redirect_to(user_page, ReqMap);
set_valid_user({error, Reason}, ReqMap) ->
    #{session_id := SessionId} = ReqMap,
    lager:info("~p: login failed due to ~p", [SessionId, Reason]),
    Desc = <<"an error occured during login">>,
    show_error_and_close_session(Desc, ReqMap);
set_valid_user(_, #{session_id := SessionId } = ReqMap) ->
    lager:info("~p: user login failed ~p", [SessionId, ReqMap]),
    Error = <<"Invalid/Unknown User">>,
    show_error_and_close_session(Error, ReqMap).

redirect_to(auth_server, #{op_id := OpenIdProviderId, session:=Session,
                           session_id:=SessionId}) ->
    {ok, OidcState} = tts_session:get_oidc_state(Session),
    {ok, OidcNonce} = tts_session:get_oidc_nonce(Session),
    ok = tts_session:set_oidc_provider(OpenIdProviderId, Session),
    {ok, Redirection} = oidcc:create_redirect_url(OpenIdProviderId,
                                                  [openid, email, profile],
                                                  OidcState, OidcNonce),
    lager:info("~p: redirecting to openid provider: ~p", [SessionId,
                                                          Redirection]),
    create_redirection(Redirection);
redirect_to(user_page, _ReqMap) ->
    UserPath = ?CONFIG(ep_user),
    create_redirection(UserPath);
redirect_to(_, _ReqMap) ->
    create_redirection("/").

show_error_and_close_session(Error, ReqMap) ->
    #{ session := Session, session_id := SessionId } = ReqMap,
    lager:info("~p: closing session due to ~p", [SessionId, Error]),
    ok = tts_session:close(Session),
    Update = #{error => Error, status => 200, cookie => clear},
    NewReqMap = maps:merge(ReqMap, Update),
    show_select_page(NewReqMap).



create_redirection(Url) ->
    #{header => [{<<"location">>, Url}], cookie => update, status => 302}.


