-module(tts).
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
-include("tts.hrl").

-export([
         login_with_oidcc/1,
         login_with_access_token/2,
         logout/1,

         does_credential_exist/2,
         does_temp_cred_exist/2,

         get_openid_provider_list/0,
         get_service_list_for/1,
         get_credential_list_for/1,
         request_credential_for/4,
         revoke_credential_for/2,

         get_access_token_for/1,
         get_display_name_for/1,

         store_temp_cred/2,
         get_temp_cred/2,

         start_full_debug/0,
         start_debug/1,
         start_debug/2,
         stop_debug/0,
         set_debug_mode/1
        ]).

login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}}}
= TokenMap) ->
    do_login(Issuer, Subject, TokenMap);
login_with_oidcc(_BadToken) ->
    lager:debug("bad token"),
    {error, bad_token}.

login_with_access_token(AccessToken, Issuer) when is_binary(AccessToken),
                                                  is_binary(Issuer) ->
    do_login(Issuer, undefined, AccessToken);
login_with_access_token(_AccessToken, _Issuer) ->
    {error, bad_token}.

do_login(Issuer, Subject0, Token0) ->
    {ok, SessPid} = tts_session_mgr:new_session(),
    try
        {Subject, Token} = case Subject0 of
                               undefined ->
                                   get_subject_update_token(Issuer, Token0);
                               _ ->
                                   {Subject0, Token0}
                           end,
        update_session(Issuer, Subject, Token, SessPid)
    catch Error:Reason ->
            logout(SessPid),
            StackTrace = erlang:get_stacktrace(),
            lager:error("login failed due to ~p:~p at ~p", [Error, Reason,
                                                            StackTrace]),
            {error, internal}
    end.


logout(Session) ->
    tts_session:close(Session).

does_credential_exist(Id, Session) ->
    {ok, #{site := #{uid :=UserId}}} =  tts_session:get_user_info(Session),
    tts_credential:exists(UserId, Id).

does_temp_cred_exist(Id, Session) ->
    {ok, UserInfo} =  tts_session:get_user_info(Session),
    tts_temp_cred:exists(Id, UserInfo).



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
    {ok, UserInfo} = tts_session:get_user_info(Session),
    #{ site := #{uid := UserId}} = UserInfo,
    {ok, ServiceList} = tts_service:get_list(UserId),
    {ok, ServiceList}.

get_credential_list_for(Session) ->
    {ok, UserInfo} = tts_session:get_user_info(Session),
    #{ site := #{uid := UserId}} = UserInfo,
    {ok, CredentialList} = tts_credential:get_list(UserId),
    {ok, CredentialList}.


request_credential_for(ServiceId, Session, Params, Interface) ->
    {ok, UserInfo} = tts_session:get_user_info(Session),
    {ok, Token} = tts_session:get_token(Session),
    {ok, SessionId} = tts_session:get_id(Session),
    true = tts_service:is_enabled(ServiceId),
    case
        tts_credential:request(ServiceId, UserInfo, Interface, Token, Params) of
        {ok, Credential, Log} ->
            [#{name := id, value:=CredId}|_] = Credential,
            lager:info("~p: requested credential ~p",
                       [SessionId, CredId]),
            {ok, Credential, Log};
        {error, Reason, Log} ->
            lager:warning("~p: credential request for ~p failed with ~p",
                          [SessionId, ServiceId, Reason]),
            {error, Reason, Log}
    end.


revoke_credential_for(CredId, Session) ->
    {ok, UserInfo} = tts_session:get_user_info(Session),
    {ok, Issuer, Subject} = tts_session:get_iss_sub(Session),
    {ok, SessionId} = tts_session:get_id(Session),
    case tts_credential:revoke(CredId, UserInfo) of
        {ok, Result, Log}  ->
            lager:info("~p: revoked credential ~p as ~p ~p",
                       [SessionId, CredId, Issuer, Subject]),
            {ok, Result, Log};
        {error, Error, Log} ->
            lager:warning("~p: revocation of credential ~p  as ~p ~p
            failed with ~p", [SessionId, CredId, Issuer, Subject, Error]),
            {error, Error, Log}
    end.




get_access_token_for(Session) ->
    {ok, #{access := #{token := AccessToken}}} = tts_session:get_token(Session),
    {ok, AccessToken}.

get_display_name_for(Session) ->
    {ok, Name} = tts_session:get_display_name(Session),
    {ok, Name}.

store_temp_cred(Credential, Session) ->
    {ok, UserInfo} = tts_session:get_user_info(Session),
    {ok, Id} = tts_temp_cred:add_cred(Credential, UserInfo),
    {ok, Id}.

get_temp_cred(Id, Session) ->
    {ok, UserInfo} = tts_session:get_user_info(Session),
    tts_temp_cred:get_cred(Id, UserInfo).

start_full_debug() ->
    %debug these modules
    ListOfModules = [ "tts_http",
                      "tts_rest",
                      "tts_oidc_client",
                      "tts_rest_cred",
                      "tts_user_cache",
                      "tts_session",
                      "tts_session_mgr",
                      "tts_idh",
                      "tts_idh_worker",
                      "tts_service",
                      "tts_credential",
                      "tts_cred_worker"
                    ],
    start_debug(ListOfModules).

set_debug_mode(OnOff) ->
    application:set_env(tts, debug_mode, OnOff).

start_debug(ListOfModules) ->
    %debug for an hour or 10000 messages
    Options = [{time, 3600000}, {msgs, 10000}],
    start_debug(ListOfModules, Options).

start_debug(ListOfModules, Options) ->
    redbug:start(ListOfModules, Options).


stop_debug() ->
    redbug:stop().


update_session(Issuer, Subject, Token, SessionPid) ->
    {ok, UserInfo} = tts_user_cache:get_user_info(Issuer, Subject),
    {ok, SessId} = tts_session:get_id(SessionPid),
    ok = tts_session:set_token(Token, SessionPid),
    ok = tts_session:set_user_info(UserInfo, SessionPid),
    ok = tts_session:set_iss_sub(Issuer, Subject, SessionPid),
    true = tts_session:is_logged_in(SessionPid),
    lager:debug("login success"),
    {ok, #{session_id => SessId, session_pid => SessionPid}}.

get_subject_update_token(Issuer, AccessToken)  ->
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    {ok, #{issuer := Issuer}} = oidcc:get_openid_provider_info(ProviderPid),
    {ok, #{sub := Subject} = OidcInfo} =
        oidcc:retrieve_user_info(AccessToken, ProviderPid),
    Token = #{access => #{token => AccessToken}, user_info => OidcInfo},
    {Subject, Token}.
