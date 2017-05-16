-module(watts).
%%
%% Copyright 2016 - 2017 SCC/KIT
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
-include("watts.hrl").

-export([
         login_with_oidcc/1,
         login_with_access_token/2,
         login_with_rsp/2,
         logout/1,
         session_with_error/1,
         session_for_rsp/1,

         does_credential_exist/2,
         does_temp_cred_exist/2,

         get_openid_provider_list/0,
         get_openid_provider_info/1,
         get_service_list_for/1,

         get_credential_list_for/1,
         request_credential_for/3,
         revoke_credential_for/2,

         get_access_token_for/1,
         get_display_name_for/1,
         get_iss_sub_for/1,
         get_iss_id_sub_for/1,

         store_temp_cred/2,
         get_temp_cred/2,

         start_debug/1,
         start_debug/2,
         stop_debug/0
        ]).

login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}},
                   cookies := Cookies} = TokenMap0) ->
    Cookie = case lists:keyfind(watts_http_util:cookie_name(), 1, Cookies) of
                 false -> undefined;
                 {_, Data} -> Data
             end,
    TokenMap = maps:remove(cookies, TokenMap0),
    case get_session_type(Cookie) of
        {ok, oidc, SessionPid} when is_pid(SessionPid) ->
            do_additional_login(Issuer, Subject, TokenMap, SessionPid);
        {ok, {rsp, _, login}, Pid}  ->
            do_rsp_additional_login(Issuer, Subject, TokenMap, Pid);
        {ok, none} ->
            do_login_if_issuer_enabled(Issuer, Subject, TokenMap)
    end;
login_with_oidcc(_BadToken) ->
    {error, bad_token}.


login_with_access_token(AccessToken, Issuer) when is_binary(AccessToken),
                                                  is_binary(Issuer) ->
    do_login_if_issuer_enabled(Issuer, undefined, AccessToken);
login_with_access_token(_AccessToken, _Issuer) ->
    {error, bad_token}.

login_with_rsp(Rsp, SessionPid) ->
    {Issuer, Subject} = watts_rsp:get_iss_sub(Rsp),
    Token = #{access => #{token => <<"RSP">>}},
    update_session(Issuer, Issuer, Subject, Token, SessionPid).


session_for_rsp(Rsp) ->
    %% ValidService = is_allowed_service(ServiceId, Rsp),
    Provider = watts_rsp:get_provider(Rsp),
    SessType = watts_rsp:session_type(Rsp),
    {ServiceId, Params} = watts_rsp:get_service_data(Rsp),
    ValidService = true,
    ProviderEnabled = not is_provider_disabled(Provider),
    NoProvider = (Provider == undefined),
    ValidProvider = NoProvider or ProviderEnabled,
    rsp_session_or_error(ValidService and ValidProvider,
                            ServiceId, Params, Provider, Rsp, SessType).

rsp_session_or_error(true, ServiceId, Params, Provider, Rsp, SessType) ->
    {ok, SessPid} = empty_session(),
    ok = watts_session:set_type(SessType, SessPid),
    ok = watts_session:set_rsp(Rsp, SessPid),
    ok = watts_session:set_redirection(ServiceId, Params, Provider,
                                       SessPid),
    {ok, SessPid};
rsp_session_or_error(false, _, _, _, _, _) ->
    {error, bad_request}.


session_with_error(Msg) ->
    {ok, SessPid} = empty_session(),
    ok = watts_session:set_error(Msg, SessPid),
    {ok, SessPid}.

empty_session() ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    false = watts_session:is_logged_in(SessPid),
    {ok, SessPid}.



do_login_if_issuer_enabled(Issuer, Subject, Token) ->
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    {ok, #{issuer := Issuer, id := Id}} =
        oidcc:get_openid_provider_info(ProviderPid),
    case is_provider_disabled(Id) of
        false ->
            new_login(Issuer, Subject, Token);
        true ->
            {error, login_disabled}
    end.

do_rsp_additional_login(Issuer, Subject, TokenMap, Pid) ->
    {ok, #{provider := Provider}} = watts_session:get_redirection(Pid),
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    Result = oidcc:get_openid_provider_info(ProviderPid),
    RspEnabled = ?CONFIG(enable_rsp),
    trigger_rsp_additional_login(RspEnabled, Provider, Result, Subject,
                                 TokenMap, Pid).


trigger_rsp_additional_login(false, _, _, _, _, _) ->
    {error, rsp_not_enabled};
trigger_rsp_additional_login(true, Provider, {ok, #{issuer := Issuer,
                                                    id := Id}},
                             Subject, TokenMap, Pid) when Provider == Id ->
    do_additional_login(Issuer, Subject, TokenMap, Pid);
trigger_rsp_additional_login(true, _Provider, _ , _Subject, _TokenMap, _Pid) ->
    {error, bad_issuer}.



new_login(Issuer, Subject, Token) ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    do_login(Issuer, Subject, Token, SessPid).

do_login(Issuer, Subject0, Token0, SessPid) ->
    SessionId = watts_session:get_id(SessPid),
    ok = update_session_type(Subject0, SessPid),
    try
        Result = retrieve_information(Issuer, Subject0, Token0, SessPid),
        case Result of
            {ok, Subject, _IssuerId, Token} ->
                update_session(Issuer, Subject, Token, SessPid);
            {error, ErrReason} ->
                logout(SessPid),
                lager:error("SESS~p login failed due to ~p",
                            [SessionId, ErrReason]),
                {error, ErrReason}
        end
    catch Error:Reason ->
            logout(SessPid),
            StackTrace = erlang:get_stacktrace(),
            lager:error("SESS~p login failed due to ~p:~p at ~p",
                        [SessionId, Error, Reason, StackTrace]),
            {error, internal}
    end.

update_session_type(undefined, SessPid) ->
    watts_session:set_type(rest, SessPid);
update_session_type(_, SessPid) ->
    watts_session:set_type(oidc, SessPid).



do_additional_login(Issuer, Subject0, Token0, SessPid) ->
    {ok, SessionId} = watts_session:get_id(SessPid),
    lager:info("SESS~p additional login as ~p at ~p",
               [SessionId, Subject0, Issuer]),
    try
        Result = retrieve_information(Issuer, Subject0, Token0, SessPid),
        case Result of
            {ok, _Subject, IssuerId, Token} ->
                ok = watts_session:add_additional_login(IssuerId, Token,
                                                       SessPid),
                return_session_info(SessPid);
            {error, ErrReason} ->
                %% TODO: show error to user
                ok = watts_session:clear_redirection(SessPid),
                lager:error("SESS~p additional login failed due to ~p",
                            [SessionId, ErrReason]),
                return_session_info(SessPid)
        end
    catch Error:Reason ->
            %% TODO: show error to user
            StackTrace = erlang:get_stacktrace(),
            lager:error("SESS~p additional login failed due to ~p:~p at ~p",
                        [SessionId, Error, Reason, StackTrace]),
            return_session_info(SessPid)
    end.


logout(Session) ->
    watts_session:close(Session).

does_credential_exist(Id, Session) ->
    {ok, UserInfo} =  watts_session:get_user_info(Session),
    watts_plugin:exists(UserInfo, Id).

does_temp_cred_exist(Id, Session) ->
    {ok, UserId} =  watts_session:get_userid(Session),
    watts_temp_cred:exists(Id, UserId).



get_openid_provider_list() ->
    {ok, OidcProvList} = oidcc:get_openid_provider_list(),
    ExtFields = fun({Id, Pid}, List) ->
                        {ok, #{description := Desc,
                               extra_config := #{priority := Priority},
                               ready := Ready,
                               issuer := Issuer
                              }} = oidcc:get_openid_provider_info(Pid),
                        case is_provider_disabled(Id) of
                            true ->
                                List;
                            false ->
                                [ #{ id => Id, desc => Desc,
                                     priority => Priority, ready => Ready,
                                     issuer => Issuer } | List]
                        end
                end,
    OpList = lists:reverse(lists:foldl(ExtFields, [], OidcProvList)),
    {ok, OpList}.

get_openid_provider_info(ProviderId) ->
    case is_provider_disabled(ProviderId) of
        false ->
            oidcc:get_openid_provider_info(ProviderId);
        true ->
            {error, login_disabled}
    end.


is_provider_disabled(ProviderId) ->
    ProviderList = ?CONFIG(provider_list),
    ProviderDisabled = fun(#{id := Id, disable_login := Disable}, Current) ->
                               case Id == ProviderId of
                                   true ->
                                       Disable;
                                   false ->
                                       Current
                               end
                       end,
    lists:foldl(ProviderDisabled, true, ProviderList).


get_service_list_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, ServiceList} = watts_service:get_list(UserInfo),
    {ok, ServiceList}.


get_credential_list_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, CredentialList} = watts_plugin:get_cred_list(UserInfo),
    {ok, CredentialList}.


request_credential_for(ServiceId, Session, Params) ->
    IFace =  case watts_session:get_type(Session) of
                 {ok, rest} -> <<"REST interface">>;
                 {ok, {rsp, _, _}}  -> <<"RSP interface">>;
                 {ok, oidc} ->  <<"Web App">>
             end,
    {ok, UserInfo} = watts_session:get_user_info(Session),
    true = watts_service:is_enabled(ServiceId),
    Result = watts_plugin:request(ServiceId, UserInfo, IFace, Params),
    handle_credential_result(Result, ServiceId, Session, Params).

handle_credential_result({ok, Credential}, ServiceId, Session, _Params) ->
    {ok, SessionId} = watts_session:get_id(Session),
    ok = watts_session:clear_additional_logins(ServiceId, Session),
    #{id := CredId} = Credential,
    lager:info("SESS~p got credential ~p for ~p",
               [SessionId, CredId, ServiceId]),
    {ok, #{result => ok, credential => Credential}};
handle_credential_result({oidc_login, #{provider := Provider, msg := UsrMsg}},
                         ServiceId, Session, Params) ->
    {ok, SessionId} = watts_session:get_id(Session),
    Msg = <<"OpenID Connect Login error, please contact the admin">>,
    DoneMsg = <<"the plugin required a login that has already been performed">>,
    OffMsg = <<"login is required, yet the needed provider is not ready">>,
    RestMsg = <<"login is required, please use the web interface">>,
    ProviderResult = oidcc:get_openid_provider_info(Provider),
    {ok, SessionType} = watts_session:get_type(Session),
    {ok, UserInfo} = watts_session:get_user_info(Session),
    Exists = watts_userinfo:has_additional_login(ServiceId, Provider, UserInfo),
    case {ProviderResult, SessionType, Exists}  of
        {{ok, #{ready := true}}, oidc, false} ->
            ok = watts_session:set_redirection(ServiceId, Params, Provider,
                                               Session),
            Url = ?CONFIG(local_endpoint),
            {ok, #{result => oidc_login,
                   oidc_login => #{ provider => Provider,
                                    url => Url,
                                   msg => UsrMsg} }
            };
        {{ok, _}, oidc, true} ->
            {error, #{result => error, user_msg => DoneMsg}};
        {{ok, _}, oidc, _} ->
            {error, #{result => error, user_msg => OffMsg}};
        {{ok, _}, rest, _} ->
            {error, #{result => error, user_msg => RestMsg}};
        _ ->
            WMsg = "SESS~p additional login request for ~p failed: ~s",
            lager:warning(WMsg, [SessionId, ServiceId, Provider]),
            {error, #{result => error, user_msg => Msg}}
    end;
handle_credential_result({error, Map}, ServiceId, Session, _Params)
  when is_map(Map) ->
    {ok, SessionId} = watts_session:get_id(Session),
    ok = watts_session:clear_additional_logins(ServiceId, Session),
    UserMsg = get_user_msg(Map),
    LogMsg = maps:get(log_msg, Map),
    WMsg = "SESS~p credential request for ~p failed: ~s",
    lager:warning(WMsg, [SessionId, ServiceId, LogMsg]),
    BadCred = #{result => error, user_msg => UserMsg},
    {error, BadCred};
handle_credential_result({error, Reason}, ServiceId, Session, _Params) ->
    {ok, SessionId} = watts_session:get_id(Session),
    ok = watts_session:clear_additional_logins(ServiceId, Session),
    BaseWMsg = "SESS~p credential request for ~p failed: ~p",
    {UMsg, WMsg}
        = case Reason of
              limit_reached ->
                  {<<"the credential limit has been reached">>,
                   BaseWMsg};
              user_not_allowed ->
                  {<<"you are not allowed to use this service">>,
                   BaseWMsg};
              service_disabled ->
                  {<<"the service you tried to use is disabled">>,
                   BaseWMsg};
              invalid_params ->
                  {<<"invalid parameter have been passed">>,
                   BaseWMsg};
              {storing, {error, not_unique_state}} ->
                  {list_to_binary(
                     io_lib:format("~s ~s",
                                   ["the service did not return a unique",
                                    "state, contact the administrator"])),
                   <<"SESS~p identical state in request ~p: ~p">>
                  };
              _ ->
                  {<<"unknown error occured, please contact the admin">>,
                   BaseWMsg}
          end,
    lager:warning(WMsg, [SessionId, ServiceId, Reason]),
    {error, #{result => error, user_msg => UMsg}}.





revoke_credential_for(CredId, Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, SessionId} = watts_session:get_id(Session),
    case watts_plugin:revoke(CredId, UserInfo) of
        {ok, #{}} ->
            lager:info("SESS~p revoked credential ~p",
                       [SessionId, CredId]),
            ok;
        {error, Map} ->
            case maps:get(log_msg, Map, undefined) of
                undefined ->
                    ok;
                LogMsg ->
                    WMsg = "SESS~p credential revoke for ~p failed: ~s",
                    lager:warning(WMsg, [SessionId, CredId, LogMsg])
            end,
            Reason = get_user_msg(Map),
            {error, Reason}
    end.




get_access_token_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, AccessToken} = watts_userinfo:return(access_token, UserInfo),
    {ok, AccessToken}.

get_display_name_for(Session) ->
    case watts_session:get_display_name(Session) of
        {ok, Name} -> {ok, Name};
        _ -> {ok, <<"">>}
    end.

get_iss_sub_for(Session) ->
    {ok, Iss, _, Sub} = get_iss_id_sub_for(Session),
    {ok, Iss, Sub}.

get_iss_id_sub_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, IssId} = watts_session:get_iss_id(Session),
    {ok, Iss, Sub} = watts_userinfo:return(issuer_subject, UserInfo),
    {ok, Iss, IssId, Sub}.

store_temp_cred(Credential, Session) ->
    {ok, UserId} = watts_session:get_userid(Session),
    {ok, Id} = watts_temp_cred:add_cred(Credential, UserId),
    {ok, Id}.

get_temp_cred(Id, Session) ->
    {ok, UserId} = watts_session:get_userid(Session),
    watts_temp_cred:get_cred(Id, UserId).

start_debug(ListOfModules) ->
    %debug for an hour or 10000 messages
    Options = [{time, 3600000}, {msgs, 10000}],
    start_debug(ListOfModules, Options).

start_debug(ListOfModules, Options) ->
    redbug:start(ListOfModules, Options).


stop_debug() ->
    redbug:stop().

update_session(Issuer, Subject, Token, SessionPid) ->
    {ok, Provider} = oidcc:find_openid_provider(Issuer),
    {ok, #{id := IssId}} = oidcc:get_openid_provider_info(Provider),
    update_session(Issuer, IssId, Subject, Token, SessionPid).


update_session(Issuer, IssId, Subject, Token, SessionPid) ->
    ok = watts_session:set_iss_sub(Issuer, Subject, SessionPid),
    true = watts_session:is_logged_in(SessionPid),
    ok = watts_session:set_iss_id(IssId, SessionPid),
    ok = watts_session:set_token(Token, SessionPid),
    {ok, SessId} = watts_session:get_id(SessionPid),
    {ok, DisplayName} = get_display_name_for(SessionPid),
    lager:info("SESS~p logged in as ~p [~p at ~p]",
               [SessId, DisplayName, Subject, Issuer]),
    return_session_info(SessionPid).


return_session_info(SessionPid) ->
    {ok, SessId} = watts_session:get_id(SessionPid),
    {ok, SessToken} = watts_session:get_sess_token(SessionPid),
    {ok, SessType} = watts_session:get_type(SessionPid),
    {ok, #{session_id => SessId, session_token => SessToken,
           session_pid => SessionPid, session_type => SessType}}.


retrieve_information(Issuer, Subject, Token, Session) ->
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    {ok, #{id := IssuerId, issuer := Issuer} = Config} =
        oidcc:get_openid_provider_info(ProviderPid),
    UserInfoResult = oidcc:retrieve_user_info(Token, ProviderPid, Subject),
    UserInfo = extract_userinfo(UserInfoResult),
    HasScope = case is_map(Token) of
                   false -> false;
                   true ->
                       maps:is_key(scope, Token)
               end,
    TokenInfo = introspect_token_if_needed(HasScope, Token, Config, Session),
    create_information_result(UserInfo, TokenInfo, Token, IssuerId).


extract_userinfo({ok, #{sub := Subject} = OidcInfo}) ->
    {ok, Subject, OidcInfo};
extract_userinfo({error, {bad_status, #{body := Body}}}) ->
    case jsone:try_decode(Body, [{object_format, map},
                                 {keys, attempt_atom}]) of
        {ok, #{error := Reason, error_description:=Desc}, _} ->
            Msg = io_lib:format("~s: ~s", [binary_to_list(Reason),
                                           binary_to_list(Desc)]),
            {error, list_to_binary(Msg)};
        {ok, #{error_description := Reason}, _} ->
            {error, Reason};
        {ok, #{error := Reason}, _} ->
            {error, Reason};
        _ ->
            {error,  Body}
    end;
extract_userinfo({error, Reason}) ->
    {error, Reason};
extract_userinfo(Unknown) ->
    {error, io_lib:format("unknown result: ~p", [Unknown])}.

introspect_token_if_needed(true, _, _, _) ->
    {ok, #{}};
introspect_token_if_needed(false, Token, #{introspection_endpoint := _,
                                      id := Id} = Config, Session) ->
    case oidcc:introspect_token(Token, Config) of
        {ok, #{active := true} = TokenInfo} ->
            {ok, TokenInfo};
        {ok, #{active := _} = TokenInfo} ->
            lager:warning("SESS ~p: Token Introspection ~p not active",
                          [Session, TokenInfo]),
            {error, token_not_active};
        {error, {bad_status, #{status := 403}}} ->
            lager:info("SESS~p provider ~p: token introspection denied",
                          [Session, Id]),
            {error, forbidden};
        {error, Reason} ->
            lager:warning("SESS~p error at token introspection: ~p", [Session,
                                                                      Reason]),
            {error, Reason}
    end ;
introspect_token_if_needed(false, _, #{id := Id}, Session) ->
    lager:debug("SESS~p provider ~p does not support token introspection",
               [Session, Id]),
    {ok, #{}}.



create_information_result({ok, Subject, OidcInfo}, TokenResult,
                          TokenMap, IssuerId)
  when is_map(TokenMap) ->
    TokenInfo =
        case TokenResult of
            {ok, Info} ->
                Info;
            {error, _} ->
                #{}
        end,
    Update = #{user_info => OidcInfo, token_info => TokenInfo},
    {ok, Subject, IssuerId, maps:merge(TokenMap, Update)};
create_information_result({ok, Subject, OidcInfo}, TokenResult,
                          AccessToken, IssuerId)
  when is_binary(AccessToken) ->
    TokenInfo =
        case TokenResult of
            {ok, Info} ->
                Info;
            {error, _Error} ->
                #{}
        end,
    TokenMap = #{access => #{token => AccessToken},
                 user_info => OidcInfo, token_info => TokenInfo},
    {ok, Subject, IssuerId, TokenMap};
create_information_result({error, Reason} , _, _, _) ->
    {error, Reason};
create_information_result(_, {error, Reason}, _, _) ->
    {error, Reason}.


get_session_type(Cookie) when is_binary(Cookie) ->
    Result =  watts_session_mgr:get_session(Cookie),
    get_session_type(Result);
get_session_type({ok, Pid}) when is_pid(Pid) ->
    {ok, Type} = watts_session:get_type(Pid),
    {ok, Type, Pid};
get_session_type(_) ->
    {ok, none}.





get_user_msg(#{user_msg := Msg}) when is_list(Msg) ->
    list_to_binary(Msg);
get_user_msg(#{user_msg := Msg}) when is_binary(Msg) ->
    Msg;
get_user_msg(#{user_msg := Msg}) ->
    list_to_binary(io_lib:format("~p", [Msg])).
