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

-export([
         get_openid_provider_list/0,
         get_service_list_for/1,
         get_credential_list_for/1,
         get_credential_for/2,
         request_credential_for/4,
         revoke_credential_for/2,

         get_access_token_for/1,
         get_display_name_for/1,

         convert_to_json/2,

         start_full_debug/0,
         start_debug/1,
         start_debug/2,
         stop_debug/0,
         set_debug_mode/1
        ]).

-export_type([
              oidc_id/0,
              user_info/0,
              cred/0
             ]).

-type oidc_id() :: { Issuer:: binary(), Subject::binary() }.

-type user_info() :: #{ uid => binary(),
                        uidNumber => integer(),
                        gidNumber => integer(),
                        homeDirectory => binary(),
                        issuer => binary(),
                        subject => binary(),
                        groups => [binary()],
                        userIds => [oidc_id()]
                      }.

-type cred() :: #{ cred_id => binary(), ctime => integer(),
                   cred_state => binary(), service_id => binary(),
                   interface => binary(), user_id => binary()
                 }.


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


get_credential_for(_CredId, _Session) -> ok.

request_credential_for(ServiceId, Session, Params, Interface) -> 
    {ok, UserInfo} = tts_session:get_user_info(Session),
    {ok, Token} = tts_session:get_token(Session),
    {ok, SessionId} = tts_session:get_id(Session),
    true = tts_service:is_enabled(ServiceId),
    case tts_credential:request(ServiceId, UserInfo, Interface, Token, Params) of
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


convert_to_json(provider_list, ProviderList) ->
    IdIssuer = fun(Map, List) ->
                       [maps:with([id, issuer], Map) | List]
               end,
    JsonList = lists:reverse(lists:foldl(IdIssuer, [], ProviderList)),
    jsx:encode(#{openid_provider_list => JsonList}).
    

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
