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
%% @doc This module is the main entry point for any kind action to happen.
%% Including logggin in, requesting and revoking of credentials.
-module(watts).
-author("Bas Wegh, Bas.Wegh<at>kit.edu").
-include("watts.hrl").

-export([
         login_with_oidcc/2,
         login_with_access_token/2,
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

-export_type([credential/0, oidc_info/0]).

-type session_info() :: #{session_pid => Session :: pid(),
                          session_type => watts_session:type()}.

-type credential() :: #{cred_id => CredId :: binary(),
                        ctime => CTime :: binary() ,
                        interface => Interface :: binary(),
                        service_id => ServiceId :: binary(),
                        cred_state => CredState :: binary()}.

-type oidc_info() :: #{ id => Id :: binary(),
                        desc => Desc :: binary() ,
                        priority => Priority :: integer(),
                        ready => Ready :: boolean(),
                        issuer => Issuer :: binary() }.

%% @doc perform a login with the openid flow through the ui (not access token).
%% there are some possible valid cases, if the Token contains the minimal
%% needed information:
%% <ul>
%% <li> a first login, then the session type is none,
%% calling {@link do_login_if_issuer_enabled/3} </li>
%% <li> an additional login, then the session type is oidc,
%% calling {@link do_additional_login/4} </li>
%% <li> an additional login at an rsp session, session type is {rsp, _, login},
%% calling {@link do_rsp_additional_login/4} </li>
%% <li> all other session types are considered an error </li>
%% </ul>
%%
%% It fails for sure if the Token does not contain the minimal needed
%% information, which is at least subject and issuer in the IdToken.
%%
%% The login at the api level with access token is performed using
%% @{link login_with_access_token/2}.
%% @see watts_session
-spec login_with_oidcc(map(),
                       {SessType :: watts_session:type(),
                        Session :: pid() | undefined })
                      -> {ok, session_info()} |
                         {error, Reason :: atom()}.
login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}}}
                 = TokenMap, {oidc, Pid} ) when is_pid(Pid) ->
    do_additional_login(Issuer, Subject, TokenMap, Pid);
login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}}}
                 = TokenMap, {{rsp, _, login}, Pid} ) when is_pid(Pid) ->
    do_rsp_additional_login(Issuer, Subject, TokenMap, Pid);
login_with_oidcc(#{id := #{claims := #{ sub := Subject, iss := Issuer}}}
                 = TokenMap, {none, _} )  ->
    do_login_if_issuer_enabled(Issuer, Subject, TokenMap);
login_with_oidcc(#{id := #{claims := #{ sub := _, iss := _}}}, _) ->
    {error, bad_session_type};
login_with_oidcc(_BadToken, _) ->
    {error, bad_token}.


%% @doc login with an access token, which is use at the API.
%% this function in turn calls {@link do_login_if_issuer_enabled/3}.
-spec login_with_access_token(AccessToken :: binary() | atom(),
                              Issuer :: binary())
                             -> {ok, session_info()} |
                                {error, Reason :: atom()}.
login_with_access_token(AccessToken, Issuer) when is_binary(AccessToken),
                                                  is_binary(Issuer) ->
    do_login_if_issuer_enabled(Issuer, undefined, AccessToken);
login_with_access_token(_AccessToken, _Issuer) ->
    {error, bad_token}.


%% @doc setup a session for an rsp, get called when performing rsp logins.
%% RSP sessions always have a primary login that is coming from the RSP.
%% If a user needs to authenticate against some OpenID Connect provider this
%% always is an additional login for an RSP session.
%% @see watts_rsp:validate_jwt_get_rsp/2
%% @see watts_http_rsp:handle/2
-spec session_for_rsp(watts_rsp:rsp()) ->
                             {ok, Session :: pid()} |
                             {error, Reason::atom()}.
session_for_rsp(Rsp) ->
    Provider = watts_rsp:get_provider(Rsp),
    SessType = watts_rsp:session_type(Rsp),
    {ServiceId, Params} = watts_rsp:get_service_data(Rsp),
    ProviderEnabled = not is_provider_disabled(Provider),
    NoProvider = no_provider_used(Provider),
    ValidProvider = NoProvider or ProviderEnabled,
    rsp_session_or_error(ValidProvider,
                            ServiceId, Params, Provider, Rsp, SessType).


%% @doc a helper function to get rid of the dialyzer error
-dialyzer({nowarn_function, no_provider_used/1}).
-spec no_provider_used(undefined | binary()) -> boolean().
no_provider_used(undefined) ->
    true;
no_provider_used(_) ->
    false.


%% @doc either setup the rsp session or return an error.
-spec rsp_session_or_error(ValidProvider :: boolean(),
                           ServiceId :: binary() | undefined,
                           Params :: map() | undefined,
                           Provider :: binary() | undefined,
                           Rsp :: watts_rsp:rsp(),
                           SessType :: watts_session:type()) ->
                             {ok, Session :: pid()} |
                             {error, Reason::atom()}.
rsp_session_or_error(true, ServiceId, Params, Provider, Rsp, SessType) ->
    {ok, SessPid} = empty_session(),
    ok = watts_session:set_type(SessType, SessPid),
    ok = watts_session:set_rsp(Rsp, SessPid),
    ok = watts_session:set_redirection(ServiceId, Params, Provider,
                                       SessPid),

    {Issuer, Subject} = watts_rsp:get_iss_sub(Rsp),
    Token = #{access => #{token => <<"RSP">>}},
    {ok, _} = update_session(Issuer, Issuer, Subject, Token, SessPid),

    {ok, UserInfo} = watts_session:get_user_info(SessPid),
    Allowed = watts_service:is_allowed(UserInfo, ServiceId),
    Enabled = watts_service:is_enabled(ServiceId),
    rsp_session_if_service_allowed(Enabled and Allowed, SessPid);
rsp_session_or_error(false, _, _, _, _, _) ->
    {error, bad_request}.

%% @doc return the rsp session, if the first parameter is true.
%% The first parameter is the boolean value if the service is allowed.
-spec rsp_session_if_service_allowed(ServiceAllowed :: boolean(),
                                     Session :: pid())
                                    -> {ok, Session :: pid()} |
                                       {error, Reason :: atom()}.
rsp_session_if_service_allowed(true, SessPid) ->
    {ok, SessPid};
rsp_session_if_service_allowed(_, _SessPid) ->
    {error, rsp_service_not_allowed}.


%% @doc create a basic session that only contains the error message.
-spec session_with_error(Msg :: binary()) -> {ok, Session:: pid()}.
session_with_error(Msg) ->
    {ok, SessPid} = empty_session(),
    ok = watts_session:set_error(Msg, SessPid),
    {ok, SessPid}.

%% @doc helper function initializing a session without user.
-spec empty_session() -> {ok, Session :: pid()}.
empty_session() ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    false = watts_session:is_logged_in(SessPid),
    {ok, SessPid}.


%% @doc start the login process but only if the issuer is enabled.
-spec do_login_if_issuer_enabled(Issuer :: binary(),
                                 Subject :: binary() | undefined,
                                 Token :: map() | binary() ) ->
                                        {ok, session_info()} |
                                        {error, Reason :: atom()}.
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


%% @doc perform an additional login via oidc for an RSP session
%% adding additional login information to the session. Enforcing
%% that the RSP is enabled.
-spec do_rsp_additional_login(Issuer :: binary(), Subject :: binary(),
                              TokenMap :: map(), Session :: pid()) ->
                               {ok, session_info()} | {error, Reason :: atom()}.
do_rsp_additional_login(Issuer, Subject, TokenMap, Pid) ->
    {ok, #{provider := Provider}} = watts_session:get_redirection(Pid),
    {ok, ProviderPid} = oidcc:find_openid_provider(Issuer),
    Result = oidcc:get_openid_provider_info(ProviderPid),
    RspEnabled = ?CONFIG(enable_rsp),
    trigger_rsp_additional_login(RspEnabled, Provider, Result, Subject,
                                 TokenMap, Pid).


%% @doc check if the RSP is enabled and trigger the additional login.

trigger_rsp_additional_login(false, _, _, _, _, _) ->
    {error, rsp_not_enabled};
trigger_rsp_additional_login(true, Provider, {ok, #{issuer := Issuer,
                                                    id := Id}},
                             Subject, TokenMap, Pid) when Provider == Id ->
    do_additional_login(Issuer, Subject, TokenMap, Pid);
trigger_rsp_additional_login(true, _Provider, _ , _Subject, _TokenMap, _Pid) ->
    {error, bad_issuer}.


%% @doc start e new login, so setting up a session is needed.
%% it is not needed for e.g. additional logins.
-spec new_login(Issuer :: binary(), Subject :: binary() | undefined,
                Token :: map() | binary()) -> {error, Reason :: atom()} |
                                              {ok, session_info()}.
new_login(Issuer, Subject, Token) ->
    {ok, SessPid} = watts_session_mgr:new_session(),
    do_login(Issuer, Subject, Token, SessPid).


%% @doc perform a first login, fetching all the information needed.
-spec do_login(Issuer :: binary(), Subject :: binary() | undefined,
               Token :: map() | binary(), Session :: pid() ) ->
                      {ok, session_info()} | {error, Reason :: atom()}.
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

%% @doc set the session type depending on the fact if the subject has been shown
%% in the token, so if it has been an Id Token or an Access Token
-spec update_session_type(undefined | binary(), pid()) -> ok.
update_session_type(undefined, SessPid) ->
    watts_session:set_type(rest, SessPid);
update_session_type(_, SessPid) ->
    watts_session:set_type(oidc, SessPid).


%% @doc perform an additional login for a user.
%% This is e.g. needed for rsps to verify the identity of a user
%% or for plugins that need additional logins/access token
-spec do_additional_login(binary(), binary(), map(), pid())
                         -> {ok, session_info()} |
                            {error, Reason :: atom()}.
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


%% @doc close the given session
-spec logout(pid()) -> ok.
logout(Session) ->
    watts_session:close(Session).

%% @doc check if the credential with the given Id exists for the user
-spec does_credential_exist(binary(), pid()) -> boolean().
does_credential_exist(Id, Session) ->
    {ok, UserInfo} =  watts_session:get_user_info(Session),
    watts_plugin:exists(UserInfo, Id).

%% @doc check if the temp credential with the given Id exists for the user
-spec does_temp_cred_exist(binary(), pid()) -> boolean().
does_temp_cred_exist(Id, Session) ->
    {ok, UserId} =  watts_session:get_userid(Session),
    watts_temp_cred:exists(Id, UserId).


%% @doc get the list of all configured openid connect provider.
-spec get_openid_provider_list() -> {ok, [ oidc_info() ]}.
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

%% @doc get the complete openid connect provider information
-spec get_openid_provider_info(binary()) -> {ok, map()}.
get_openid_provider_info(ProviderId) ->
    case is_provider_disabled(ProviderId) of
        false ->
            oidcc:get_openid_provider_info(ProviderId);
        true ->
            {error, login_disabled}
    end.


%% @doc return if a provider is disabled, returns true if not found.
-spec is_provider_disabled(ProviderId :: binary()) -> boolean().
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


%% @doc returns the list of services for the given Session
-spec get_service_list_for(Session :: pid()) -> {ok, [Service::map()]}.
get_service_list_for(Session) ->
    {ok, SessType} = watts_session:get_type(Session),
    return_service_list(Session, SessType).

%% @doc returns the list of services for the given Session unless it is RSP
-spec return_service_list(Session :: pid(), Type :: atom() | {rsp, _, _})
                          -> {ok, [Service::map()]}.
return_service_list(Session, Type)
  when Type == oidc; Type == rest->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, ServiceList} = watts_service:get_list(UserInfo),
    {ok, ServiceList};
return_service_list(_, _) ->
    {ok, []}.


%% @doc get the list of credentials for the given session
-spec get_credential_list_for(Session :: pid()) -> {ok, [credential()]}.
get_credential_list_for(Session) ->
    {ok, SessType} = watts_session:get_type(Session),
    return_credential_list(Session, SessType).

%% @doc get the list of credentials for the given session unless it is RSP
-spec return_credential_list(Session :: pid(),  Type :: atom() | tuple()) ->
                                    {ok, [credential()]}.
return_credential_list(Session,  Type)
  when Type == oidc; Type == rest->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, CredentialList} = watts_plugin:get_cred_list(UserInfo),
    {ok, CredentialList};
return_credential_list(_,  _) ->
    {ok, []}.


%% @doc start a translation request at the given Service for the given Session.
%% This is using the params passed.
%% A plugin will be triggered to run with all the needed parameter.
%% After running the plugin the result will get parsed and send to the user.
%% @see watts_plugin:request/4
%% @see watts_plugin
%% @see watts_plugin_runner
-spec request_credential_for(ServiceId :: binary(), Session :: pid(),
                             Params :: map()) -> {ok, map()} | {error, map()}.
request_credential_for(ServiceId, Session, Params) ->
    IFace =  get_interface_description(watts_session:get_type(Session)),
    {ok, UserInfo} = watts_session:get_user_info(Session),
    true = watts_service:is_enabled(ServiceId),
    Result = watts_plugin:request(ServiceId, UserInfo, IFace, Params),
    handle_credential_result(Result, ServiceId, Session, Params).


%% @doc convert interface type in human readable string.
-spec get_interface_description({ok, atom() | tuple()}) -> binary().
get_interface_description({ok, oidc}) ->
    <<"Web App">>;
get_interface_description({ok, rest}) ->
    <<"REST interface">>;
get_interface_description({ok, {rsp, _, _}}) ->
    true = ?CONFIG(enable_rsp),
    <<"RSP interface">>.


%% @doc handle the result of a translation.
%% this is the result part of the {@link request_credential_for/3} call.
-spec handle_credential_result(watts_plugin:result(), binary(), pid(), map())
                              -> {ok, #{result => ok , credential => credential()}} |
                                 {ok, #{result => oidc_login , oidc_login => map()}} |
                                 {error, #{result => error, user_msg => binary()}}.
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
    {UMsg, WMsg} = credential_error_message(Reason),
    lager:warning(WMsg, [SessionId, ServiceId, Reason]),
    {error, #{result => error, user_msg => UMsg}}.

%% @doc convert the error to readable messages for log and user
-spec credential_error_message(Reason :: atom() | tuple() | any())
                              -> {binary(), string()}.
credential_error_message(Reason) ->
    BaseWMsg = "SESS~p credential request for ~p failed: ~p",
    credential_error_message(Reason, BaseWMsg).


%% @doc convert the error to readable messages for log and user, with default
-spec credential_error_message(Reason :: atom() | tuple(),
                              DefaultMessage :: string())
                              -> {binary(), string()}.
credential_error_message(limit_reached, BaseWMsg) ->
    {<<"the credential limit has been reached">>, BaseWMsg};
credential_error_message(user_not_allowed, BaseWMsg) ->
    {<<"you are not allowed to use this service">>, BaseWMsg};
credential_error_message(service_disabled, BaseWMsg) ->
    {<<"the service you tried to use is disabled">>, BaseWMsg};
credential_error_message(invalid_params, BaseWMsg) ->
    {<<"invalid parameter have been passed">>, BaseWMsg};
credential_error_message({storing, {error, not_unique_state}}, _) ->
    {<<"the service did not return a unique state, contact the administrator">>,
     "SESS~p identical state in request ~p: ~p"};
credential_error_message(_, BaseWMsg) ->
    {<<"unknown error occured, please contact the admin">>, BaseWMsg}.



%% @doc revoke the credential witht the given id for the user from the session.
-spec revoke_credential_for(binary(), pid()) -> ok | {error, Reason :: atom()}.
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


%% @doc return the access token from a session
-spec get_access_token_for(pid()) -> {ok, binary()}.
get_access_token_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, AccessToken} = watts_userinfo:return(access_token, UserInfo),
    {ok, AccessToken}.

%% @doc return the display name of a session
-spec get_display_name_for(pid()) -> {ok, binary()}.
-dialyzer({nowarn_function, get_display_name_for/1}).
get_display_name_for(Session) ->
    case watts_session:get_display_name(Session) of
        {ok, Name} -> {ok, Name};
        _ -> {ok, <<"">>}
    end.

%% @doc return the issuer and subject of a session
-spec get_iss_sub_for(pid()) -> {ok, binary(), binary()}.
get_iss_sub_for(Session) ->
    {ok, Iss, _, Sub} = get_iss_id_sub_for(Session),
    {ok, Iss, Sub}.

%% @doc return the issuer, its id, and subject of a session
-spec get_iss_id_sub_for(pid()) -> {ok, binary(), binary(), binary()}.
get_iss_id_sub_for(Session) ->
    {ok, UserInfo} = watts_session:get_user_info(Session),
    {ok, IssId} = watts_session:get_iss_id(Session),
    {ok, Iss, Sub} = watts_userinfo:return(issuer_subject, UserInfo),
    {ok, Iss, IssId, Sub}.

%% @doc store a credential temporary in a gen_server in RAM
-spec store_temp_cred(credential(), pid()) -> {ok, binary()}.
store_temp_cred(Credential, Session) ->
    {ok, UserId} = watts_session:get_userid(Session),
    {ok, Id} = watts_temp_cred:add_cred(Credential, UserId),
    {ok, Id}.

%% @doc get a temp credential by its id and for the user
%% this also deletes the stored credential
-spec get_temp_cred(binary(), pid()) -> {ok, credential()} | {error, atom()}.
get_temp_cred(Id, Session) ->
    {ok, UserId} = watts_session:get_userid(Session),
    watts_temp_cred:get_cred(Id, UserId).

%% @doc start debugging with defaults (one hour, 10000 messages)
-spec start_debug([string()]) -> {ok, any()}.
start_debug(ListOfModules) ->
    %debug for an hour or 10000 messages
    Options = [{time, 3600000}, {msgs, 10000}],
    start_debug(ListOfModules, Options).

%% @doc start debugging, with options
-spec start_debug([string()], [any()]) -> {ok, any()}.
start_debug(ListOfModules, Options) ->
    redbug:start(ListOfModules, Options).


%% @doc stop debugging
-spec stop_debug() -> stopped | not_started.
stop_debug() ->
    redbug:stop().

%% @doc update the session with issuer, subject and maybe more
-spec update_session(binary(), binary(), binary() | map(), pid())
                    -> {ok, session_info()}.
update_session(Issuer, Subject, Token, SessionPid) ->
    {ok, Provider} = oidcc:find_openid_provider(Issuer),
    {ok, #{id := IssId}} = oidcc:get_openid_provider_info(Provider),
    update_session(Issuer, IssId, Subject, Token, SessionPid).


%% @doc update the session with issuer, its id, subject and maybe more
-spec update_session(binary(), binary(), binary(), binary() | map(), pid())
                    -> {ok, session_info()}.
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


%% @doc generate a session description map from the given session pid.
-spec return_session_info(Session :: pid()) -> {ok, session_info()}.
return_session_info(SessionPid) ->
    {ok, SessType} = watts_session:get_type(SessionPid),
    {ok, #{session_pid => SessionPid, session_type => SessType}}.

%% @doc retrieve user information at the oidc provider
-spec retrieve_information(binary(), binary(), map(), pid())
                          -> {ok, binary(), binary(), map()} | {error, atom()}.
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

%% @doc extract user information out of the response from the oidc provider
-spec extract_userinfo({ok, map()} | {error, any()})
                      -> {ok, binary(), map()} | {error, any()} .
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

%% @doc perfom token introspection if needed
-spec introspect_token_if_needed(boolean(), map(), map(), pid())
                                -> {ok, map()} | {error, atom()}.
-dialyzer({nowarn_function, introspect_token_if_needed/4}).
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


%% @doc create the resul for the user info call
-spec create_information_result({ok, binary(), map()} | {error, atom()} ,
                                {ok, map()} | {error, atom()} ,
                                map() | binary(), binary())
                          -> {ok, binary(), binary(), map()} | {error, atom()}.
-dialyzer({nowarn_function, create_information_result/4}).
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


%% @doc extract the user message from the map
-spec get_user_msg(map()) -> binary().
get_user_msg(#{user_msg := Msg}) when is_list(Msg) ->
    list_to_binary(Msg);
get_user_msg(#{user_msg := Msg}) when is_binary(Msg) ->
    Msg;
get_user_msg(#{user_msg := Msg}) ->
    list_to_binary(io_lib:format("~p", [Msg])).
