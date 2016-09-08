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

handle(#{method := post, logged_in := true} = ReqMap) ->
    handle_user_action(ReqMap);
handle(#{method := get, logged_in := true, session := Session}) ->
    show_user_page(Session, false, [], false);
handle(ReqMap) ->
    show_select_page(ReqMap).

handle_user_action(#{body_qs:=#{action := request}} = ReqMap) ->
    request_credential(ReqMap);
handle_user_action(#{body_qs:=#{action := revoke}} = ReqMap) ->
    revoke_credential(ReqMap);
handle_user_action(#{body_qs:=#{action := logout}, session:=Session}
                   = ReqMap) ->
    ok = tts:logout(Session),
    maps:put(cookie, clear, redirect_to(ep_main, ReqMap));
handle_user_action(#{session := Session}) ->
    show_user_page(Session, false, [], false).


request_credential(#{session := Session,
                     body_qs:= #{ service_id:=ServiceId}}) ->
    Iface = <<"web interface">>,
    Result = tts:request_credential_for(ServiceId, Session, [], Iface),
    show_credential_request_result(Result, Session);
request_credential(#{session := Session}) ->
    Error = <<"Credential Request failed">>,
    show_user_page(Session, false, [], Error).


revoke_credential(#{session := Session,
                    body_qs:= #{ credential_id:=CredId}}) ->
    Result = tts:revoke_credential_for(CredId, Session),
    show_credential_revoke_result(Result, Session).

show_credential_request_result({ok, Credential, Log}, Session) ->
            show_user_page(Session, Credential, Log, false);
show_credential_request_result({error, _Reason, Log}, Session) ->
            ErrMsg = <<"failed to request credential">>,
            show_user_page(Session, false, Log, ErrMsg).

show_credential_revoke_result({ok, _Result, Log}, Session) ->
            show_user_page(Session, false, Log, false);
show_credential_revoke_result({error, _Error, Log}, Session) ->
            ErrMsg = <<"failed to revoke credential">>,
            show_user_page(Session, false, Log, ErrMsg).

show_user_page(Session, Credential, Log, Error) ->
    {ok, ServiceList} = tts:get_service_list_for(Session),
    {ok, CredentialList} = tts:get_credential_list_for(Session),
    {ok, AccessToken} = tts:get_access_token_for(Session),
    {ok, Name} = tts:get_display_name_for(Session),

    {ok, Version} = application:get_key(tts, vsn),
    Params = [
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
    {ok, Body} = tts_main_dtl:render(Params),
    #{body => Body, status => 200, cookie => update}.

show_select_page(ReqMap) ->
    {ok, OpList} = tts:get_openid_provider_list(),
    Error = maps:get(error, ReqMap, undefined),
    {ok, Version} = application:get_key(tts, vsn),
    {ok, Body} = tts_main_dtl:render([{ep_redirect, ?CONFIG(ep_oidc)},
                                      {oidc_op_list, OpList},
                                      {version, Version},
                                      {error, Error},
                                      {configured, true}]),
    Cookie = maps:get(cookie, ReqMap, update),
    #{body => Body, status => 200, cookie => Cookie}.

redirect_to(user_page, _ReqMap) ->
    UserPath = ?CONFIG(ep_user),
    create_redirection(UserPath);
redirect_to(_, _ReqMap) ->
    create_redirection("/").

create_redirection(Url) ->
    #{header => [{<<"location">>, Url}], cookie => update, status => 302}.
