-module(watts).
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
-include("watts.hrl").

-export([
         login_with_oidcc/1,
         login_with_access_token/2,
         logout/1,
         session_with_error/1,

         does_credential_exist/2,
         does_temp_cred_exist/2,

         get_openid_provider_list/0,
         get_service_list_for/1,
         get_credential_list_for/1,
         request_credential_for/4,
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

login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}}}
= TokenMap) ->
    do_login(Issuer, Subject, TokenMap);
login_with_oidcc(_BadToken) ->
    {error, bad_token}.

login_with_access_token(AccessToken, Issuer) when is_binary(AccessToken),
                                                  is_binary(Issuer) ->
    do_login(Issuer, undefined, AccessToken);
login_with_access_token(_AccessToken, _Issuer) ->
    {error, bad_token}.

session_with_error(Msg) ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    ok = watts_session:set_error(Msg, SessPid),
    false = watts_session:is_logged_in(SessPid),
    {ok, SessPid}.


do_login(Issuer, Subject0, Token0) ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    SessionId = watts_session:get_id(SessPid),
    try
        Result = retrieve_information(Issuer, Subject0, Token0, SessPid),
        case Result of
            {ok, Subject, Token} ->
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
                               ready := Ready,
                               issuer := Issuer
                              }} =
                            oidcc:get_openid_provider_info(Pid),
                        [ #{ id => Id, desc => Desc, ready => Ready,
                             issuer => Issuer } | List]
                end,
    OpList = lists:reverse(lists:foldl(ExtFields, [], OidcProvList)),
    {ok, OpList}.


get_service_list_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, ServiceList} = watts_service:get_list(UserInfo),
    {ok, ServiceList}.

get_credential_list_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, CredentialList} = watts_plugin:get_cred_list(UserInfo),
    {ok, CredentialList}.


request_credential_for(ServiceId, Session, Params, Interface) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, SessionId} = watts_session:get_id(Session),
    true = watts_service:is_enabled(ServiceId),
    case watts_plugin:request(ServiceId, UserInfo, Interface, Params) of
        {ok, Credential} ->
            #{id := CredId} = Credential,
            lager:info("SESS~p got credential ~p for ~p",
                       [SessionId, CredId, ServiceId]),
            {ok, #{result => ok, credential => Credential}};
        {error, Map}
          when is_map(Map)->
            case maps:get(log_msg, Map, undefined) of
                undefined ->
                    ok;
                LogMsg ->
                    WMsg = "SESS~p credential request for ~p failed: ~s",
                    lager:warning(WMsg, [SessionId, ServiceId, LogMsg])
            end,
            UserMsg = get_user_msg(Map),
            BadCred = #{result => error, user_msg => UserMsg},
            {error, BadCred};
        {error, Reason} ->
            Msg = case Reason of
                      limit_reached ->
                          <<"the credential limit has been reached">>;
                      user_not_allowed ->
                          <<"you are not allowed to use this service">>;
                      service_disabled ->
                          <<"the service you tried to use is disabled">>;
                      invalid_params ->
                          <<"invalid parameter have been passed">>;
                      _ ->
                          <<"unknown error occured, please contact the admin">>
                  end,
            WMsg = "SESS~p credential request for ~p failed: ~p",
            lager:warning(WMsg, [SessionId, ServiceId, Reason]),
            {error, #{result => error, user_msg => Msg}}
    end.


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
    {ok, SessId} = watts_session:get_id(SessionPid),
    {ok, SessToken} = watts_session:get_sess_token(SessionPid),
    {ok, Provider} = oidcc:find_openid_provider(Issuer),
    {ok, #{id := IssId}} = oidcc:get_openid_provider_info(Provider),

    ok = watts_session:set_iss_sub(Issuer, Subject, SessionPid),
    true = watts_session:is_logged_in(SessionPid),
    ok = watts_session:set_iss_id(IssId, SessionPid),
    ok = watts_session:set_token(Token, SessionPid),
    {ok, DisplayName} = get_display_name_for(SessionPid),
    lager:info("SESS~p logged in as ~p [~p at ~p]",
               [SessId, DisplayName, Subject, Issuer]),

    {ok, #{session_id => SessId, session_token => SessToken,
           session_pid => SessionPid}}.


retrieve_information(Issuer, Subject, Token, Session) ->
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    {ok, #{issuer := Issuer} = Config} =
        oidcc:get_openid_provider_info(ProviderPid),
    UserInfoResult = oidcc:retrieve_user_info(Token, ProviderPid, Subject),
    UserInfo = extract_userinfo(UserInfoResult),
    TokenInfo = introspect_token_if_possible(Token, Config, Session),
    create_information_result(UserInfo, TokenInfo, Token).


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

introspect_token_if_possible(Token, #{introspection_endpoint := _,
                                      id := Id} = Config, Session) ->
    case oidcc:introspect_token(Token, Config) of
        {ok, #{active := true} = TokenInfo} ->
            lager:debug("SESS ~p: Token Introspection ~p", [Session,
                                                            TokenInfo]),
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
introspect_token_if_possible(_, #{id := Id}, Session) ->
    lager:debug("SESS~p provider ~p does not support token introspection",
               [Session, Id]),
    {ok, #{}}.



create_information_result({ok, Subject, OidcInfo}, TokenResult, TokenMap)
  when is_map(TokenMap) ->
    TokenInfo =
        case TokenResult of
            {ok, Info} ->
                Info;
            {error, _} ->
                #{}
        end,
    Update = #{user_info => OidcInfo, token_info => TokenInfo},
    {ok, Subject, maps:merge(TokenMap, Update)};
create_information_result({ok, Subject, OidcInfo}, TokenResult, AccessToken)
  when is_binary(AccessToken) ->
    TokenInfo =
        case TokenResult of
            {ok, Info} ->
                Info;
            {error, _Error} ->
                #{}
        end,
    TokenMap = #{access_token => #{token => AccessToken},
                 user_info => OidcInfo, token_info => TokenInfo},
    {ok, Subject, TokenMap};
create_information_result({error, Reason} , _, _) ->
    {error, Reason};
create_information_result(_, {error, Reason}, _) ->
    {error, Reason}.



get_user_msg(#{user_msg := Msg}) when is_list(Msg) ->
    list_to_binary(Msg);
get_user_msg(#{user_msg := Msg}) when is_binary(Msg) ->
    Msg;
get_user_msg(#{user_msg := Msg}) ->
    list_to_binary(io_lib:format("~p", [Msg]));
get_user_msg(_) ->
    <<>>.
