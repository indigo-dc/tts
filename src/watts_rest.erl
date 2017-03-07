-module(watts_rest).
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


-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([allow_missing_post/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([malformed_request/2]).
-export([resource_exists/2]).
-export([get_json/2]).
-export([post_json/2]).
-export([delete_resource/2]).

%%
%% REST implementation
%%

-define(LATEST_VERSION, 2).

%
% list of API methods:
% GET /oidcp/
% GET /service/
% GET /credential
% POST /credential
% DELETE /credential/$ID

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-record(state, {
          method = undefined,
          version = undefined,
          type = undefined,
          id = undefined,

          token = undefined,
          issuer = undefined,
          json = undefined,
          session_pid = undefined,
          cookie_based = false
         }).

rest_init(Req, _Opts) ->
    Req2 = cowboy_req:set_resp_header(<<"Cache-control">>, <<"no-cache">>, Req),
    {ok, Req2, #state{}}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>]
     , Req, State}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

malformed_request(Req, State) ->
    CookieName = watts_http_util:cookie_name(),
    {CookieSessionToken, Req2} = cowboy_req:cookie(CookieName, Req),
    CookieSession = watts_session_mgr:get_session(CookieSessionToken),

    {PathInfo, Req3} = cowboy_req:path_info(Req2),
    {InToken, Req4} = cowboy_req:header(<<"authorization">>, Req3),
    {HIssuer, Req5} = cowboy_req:header(<<"x-openid-connect-issuer">>,
                                         Req4),
    {InVersion, InIssuer, InType, InId, HeaderUsed} =
        case {PathInfo, HIssuer} of
            {[V, Iss, T, Id], undefined} ->
                {V, Iss, T, Id, false};
            {[V, Iss, T], undefined} ->
                {V, Iss, T, undefined, false};
            {[V, T], undefined} ->
                {V, undefined, T, undefined, false};
            {_, undefined} ->
                {no_version, undefined, undefined, undefined, false};
            {[V, T], Iss} ->
                {V, Iss, T, undefined, true};
            {[V, T, Id], Iss} ->
                {V, Iss, T, Id, true};
            _ ->
                {no_version, undefined, undefined, undefined, false}
        end,
    {Res, ContentType, Req6} = cowboy_req:parse_header(<<"content-type">>,
                                                       Req5),
    {Method, Req7} = cowboy_req:method(Req6),
    {ok, InBody, Req8} = cowboy_req:body(Req7),

    {Result, NewState} = is_malformed(Method, {Res, ContentType}, InVersion,
                                      InType , InId, InBody, InToken,
                                      InIssuer, HeaderUsed, CookieSession,
                                      State),
    Req99 =
        case Result of
            true ->
                Msg = <<"Bad request, please check all parameter">>,
                Body = jsone:encode(#{result => error, user_msg => Msg}),
                cowboy_req:set_resp_body(Body, Req8);
            false ->
                Req8
        end,
    {Result, Req99, NewState}.


is_authorized(Req, #state{type=oidcp} = State) ->
    {true, Req, State};
is_authorized(Req, #state{type=info} = State) ->
    {true, Req, State};
is_authorized(Req, #state{type=logout} = State) ->
    {true, Req, State};
is_authorized(Req, #state{type=Type, session_pid=Pid} = State)
  when is_pid(Pid) ->
    ValidType = lists:member(Type, [oidcp, info, logout, service, credential,
                                    cred_data, access_token]),
    LoggedIn = watts_session:is_logged_in(Pid),
    case {ValidType, LoggedIn} of
        {false, _} ->
            Msg = list_to_binary(io_lib:format("unsupported path ~p", [Type])),
            Body = jsone:encode(#{result => error, user_msg => Msg}),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {{false, <<"authorization">>}, Req1, State};
        {_, false} ->
            Msg = <<"seems like the session expired">>,
            Body = jsone:encode(#{result => error, user_msg => Msg}),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {{false, <<"authorization">>}, Req1, State};
        {true, true} ->
            {true,  Req, State}
    end;
is_authorized(Req, #state{type=Type, token=Token, issuer=Issuer,
                          session_pid=undefined} = State)
  when Type==service; Type==credential; Type==cred_data; Type == oidcp ;
       Type == info ->
    case watts:login_with_access_token(Token, Issuer) of
        {ok, #{session_pid := SessionPid}} ->
            {true, Req, State#state{session_pid = SessionPid}};
        {error, internal} ->
            Msg = <<"Authorization failed, please check the access token">>,
            Body = jsone:encode(#{result => error, user_msg => Msg}),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {{false, <<"authorization">>}, Req1, State};
        {error, Reason} ->
            Body = jsone:encode(#{result => error, user_msg => Reason}),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {{false, <<"authorization">>}, Req1, State}

    end;
is_authorized(Req, State) ->
    Msg = <<"invalid token has been received">>,
    Body = jsone:encode(#{result => error, user_msg => Msg}),
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {{false, <<"authorization">>}, Req1, State}.


content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, get_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, post_json }
     ], Req, State}.

resource_exists(Req, #state{type=Type, id=undefined} = State)
    when Type == oidcp; Type == info; Type == logout; Type == service;
         Type == credential; Type == access_token ->
    {true, Req, State};
resource_exists(Req, #state{type=credential, id=Id, session_pid=Session}
                = State) ->
    Exists = watts:does_credential_exist(Id, Session),
    {Exists, Req, State};
resource_exists(Req, #state{type=cred_data, id=Id, session_pid=Session}
                = State) ->
    Exists = watts:does_temp_cred_exist(Id, Session),
    {Exists, Req, State};
resource_exists(Req, State) ->
    Msg = <<"resource not found">>,
    Body = jsone:encode(#{result => error, user_msg => Msg}),
    Req1 = cowboy_req:set_resp_body(Body, Req),
    {false, Req1, State}.

delete_resource(Req, #state{type=credential,
                            id=CredentialId, session_pid=Session}=State) ->
    {Result, Req2}  =
        case watts:revoke_credential_for(CredentialId, Session) of
            ok ->
                Body = jsone:encode(#{result => ok}),
                Req1 = cowboy_req:set_resp_body(Body, Req),
                {true, Req1};
            {error, Msg} ->
                Body = jsone:encode(#{result => error, user_msg => Msg}),
                Req1 = cowboy_req:set_resp_body(Body, Req),
                {false, Req1}
        end,
    ok = end_session_if_rest(State),
    {Result, Req2, State#state{session_pid=undefined}}.


get_json(Req, #state{version=Version, type=Type, id=Id, method=get,
                     session_pid=Session} = State) ->
    Result = perform_get(Type, Id, Session, Version),
    ok = end_session_if_rest(State),
    {ok, Req2} = update_cookie_if_used(Req, State),
    {Result, Req2, State#state{session_pid=undefined}}.


post_json(Req, #state{version=Version, type=Type, id=Id, method=post,
                      session_pid=Session, json=Json,
                      cookie_based=CookieBased} = State) ->
    {Req1, Result} = perform_post(Req, Type, Id, Json, Session, CookieBased,
                                  Version),
    ok = end_session_if_rest(State),
    {ok, Req2} = update_cookie_if_used(Req1, State),
    {Result, Req2, State#state{session_pid=undefined}}.

perform_get(service, undefined, Session, Version) ->
    {ok, ServiceList} = watts:get_service_list_for(Session),
    Keys = case Version of
               1 -> [id, type, host, port];
               _ -> [id, description, enabled, cred_count, cred_limit,
                     limit_reached, params, authorized, authz_tooltip,
                     pass_access_token]
           end,
    return_json_service_list(ServiceList, Keys);
perform_get(oidcp, _, _, 1) ->
    {ok, OIDCList} = watts:get_openid_provider_list(),
    return_json_oidc_list(OIDCList);
perform_get(oidcp, _, _, _) ->
    {ok, OIDCList} = watts:get_openid_provider_list(),
    jsone:encode(#{openid_provider_list => OIDCList});
perform_get(info, undefined, Session, _) ->
    {LoggedIn, DName, IssId, Error}  =
        case is_pid(Session) of
            false ->
                {false, <<"">>, <<"">>, <<"">>};
            true ->
                {ok, Name} = watts:get_display_name_for(Session),
                {ok, _Iss, Id, _Sub} = watts:get_iss_id_sub_for(Session),
                {ok, Err} = watts_session:get_error(Session),
                {watts_session:is_logged_in(Session), Name, Id, Err}
        end,
    {ok, Version} = ?CONFIG_(vsn),
    Redirect = io_lib:format("~s~s", [?CONFIG(ep_main), "oidc"]),
    EnableDocs = ?CONFIG(enable_docs),
    Info = #{version => list_to_binary(Version),
             redirect_path => list_to_binary(Redirect),
             error => Error,
             logged_in => LoggedIn,
             display_name => DName,
             issuer_id => IssId,
             documentation => EnableDocs
            },
    jsone:encode(Info);
perform_get(logout, undefined, undefined, _) ->
    jsone:encode(#{result => ok});
perform_get(logout, undefined, Session, _) ->
    ok = perform_logout(Session),
    jsone:encode(#{result => ok});
perform_get(access_token, undefined, Session, _) ->
    {ok, AccessToken} = watts:get_access_token_for(Session),
    {ok, Iss, Id, Sub} = watts:get_iss_id_sub_for(Session),
    jsone:encode(#{access_token => AccessToken,
                   issuer => Iss,
                   subject => Sub,
                   issuer_id => Id
                  });
perform_get(credential, undefined, Session, Version) ->
    {ok, CredList} = watts:get_credential_list_for(Session),
    return_json_credential_list(CredList, Version);
perform_get(cred_data, Id, Session, Version) ->
    case watts:get_temp_cred(Id, Session) of
        {ok, Cred} ->
            return_json_credential(Cred, Version);
        _ ->
            Msg = <<"Sorry, the requested data was not found">>,
            jsone:encode(#{result => error, user_msg => Msg})
    end.

perform_post(Req, credential, undefined, #{service_id:=ServiceId} = Data,
             Session, CookieBased, Ver) ->
    IFace =  case CookieBased of
                 false -> <<"REST interface">>;
                 true ->  <<"Web App">>
             end,
    Params = maps:get(params, Data, #{}),
    case  watts:request_credential_for(ServiceId, Session, Params, IFace) of
        {ok, CredData} ->
            {ok, Id} = watts:store_temp_cred(CredData, Session),
            {ok, _Iss, IssuerId, _Sub} = watts:get_iss_id_sub_for(Session),
            Url = id_to_url(Id, IssuerId, Ver),
            {Req, {true, Url}};
        {error, ErrorInfo} ->
            Body = jsone:encode(ErrorInfo),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {Req1, false};
        _Other ->
            UserMsg = "An internal error occured, please contact the admin.",
            Body = jsone:encode(#{result => error, user_msg => UserMsg}),
            Req1 = cowboy_req:set_resp_body(Body, Req),
            {Req1, false}
    end.

return_json_service_list(Services, Keys) ->
    Extract = fun(Map0, List) ->
                      CredLimit = case maps:get(cred_limit, Map0) of
                                      infinite ->
                                          -1;
                                      Num when is_integer(Num), Num >= 0 ->
                                          Num;
                                      _ -> 0
                                  end,
                      Update = #{type => none, host => localhost,
                                port => <<"1234">>},
                      Map = maps:put(cred_limit, CredLimit, Map0),
                      [ maps:with(Keys, maps:merge(Update, Map)) | List]
              end,
    List = lists:reverse(lists:foldl(Extract, [], Services)),
    jsone:encode(#{service_list => List}).

return_json_oidc_list(Oidc) ->
    Id = fun(OidcInfo, List) ->
                 case OidcInfo of
                 #{issuer := Issuer, id := Id, ready := true}  ->
                         [#{ id => Id, issuer => Issuer} | List];
                     _ -> List
                 end
         end,
    List = lists:reverse(lists:foldl(Id, [], Oidc)),
    jsone:encode(#{openid_provider_list => List}).

return_json_credential(Cred, 1) ->
    #{credential :=
          #{id := Id,
            entries := Entries
           }
     } = Cred,
    IdEntry = #{name => id, type => text, value => Id},
    jsone:encode([ IdEntry | Entries ]);
return_json_credential(Cred, _) ->
    jsone:encode(Cred).




return_json_credential_list(Credentials, Version)->
    Keys = [cred_id, ctime, interface, service_id],
    Adjust =
        fun(Cred0, List) ->
                Cred = maps:with(Keys, Cred0),
                case Version of
                    1 ->
                        [ #{ id => maps:put(cred_state, hidden, Cred)} | List];
                    _ ->
                        [ Cred | List]
                end
        end,
    List = lists:reverse(lists:foldl(Adjust, [], Credentials)),
    jsone:encode(#{credential_list => List}).

is_malformed(InMethod, InContentType, InVersion, InType, InId, InBody, InToken,
             InIssuer, HeaderUsed, InCookieSession, State) ->
    Version = verify_version(InVersion),
    Type = verify_type(InType),
    Id = verify_id(InId),
    Token = verify_token(InToken),
    Issuer = verify_issuer(InIssuer),
    CookieSession = verify_session(InCookieSession),
    Method = verify_method(InMethod),
    ContentType = verify_content_type(InContentType),
    Body = verify_body(InBody),
    case is_bad_version(Version, HeaderUsed) of
        true -> {true, State#state{method=Method, version=Version, type=Type,
                                   id=Id, token=Token, issuer=Issuer,
                                   session_pid=CookieSession, json=Body}};
        false -> Result = is_malformed(Method, ContentType, Type, Id, Issuer,
                                       Body),
                 {Result, State#state{method=Method, version=Version, type=Type,
                                      id=Id, token=Token, issuer=Issuer,
                                      session_pid=CookieSession, json=Body,
                                      cookie_based = is_pid(CookieSession) }}
    end.

verify_version(<<"latest">>) ->
    ?LATEST_VERSION;
verify_version(<< V:1/binary, Version/binary >>) when V==<<"v">>; V==<<"V">> ->
     safe_binary_to_integer(Version);
verify_version(_) ->
    0.

verify_token(<< Prefix:7/binary, Token/binary >>) when
      Prefix == <<"Bearer ">> ->
    Token;
verify_token(Token) when is_binary(Token) ->
    bad_token;
verify_token(Token) when is_atom(Token) ->
    Token.

verify_content_type({ok, {<<"application">>, <<"json">>, _}}) ->
    json;
verify_content_type({ok, undefined}) ->
    undefined;
verify_content_type({undefined, _}) ->
    undefined;
verify_content_type(_) ->
    unsupported.


verify_issuer(undefined) ->
    undefined;
verify_issuer(Issuer) when is_binary(Issuer) ->
    case watts:get_openid_provider_info(Issuer) of
        {ok, #{issuer := IssuerUrl}} ->
            IssuerUrl;
        _ ->
            case (byte_size(Issuer) > 8) andalso
                 (binary_part(Issuer, {0, 8}) == <<"https://">>) of
                true -> Issuer;
                false -> bad_issuer
            end
    end;
verify_issuer(_Issuer)  ->
    bad_issuer.


verify_session({ok, Pid}) when is_pid(Pid) ->
    Pid;
verify_session(_) ->
    undefined.


verify_method(<<"GET">>) ->
    get;
verify_method(<<"POST">>) ->
    post;
verify_method(<<"DELETE">>) ->
    delete.

verify_body([]) ->
    undefined;
verify_body(Data) ->
    case jsone:try_decode(Data, [{object_format, map}, {keys, attempt_atom}]) of
        {ok, Json, _} ->
            Json;
        _ ->
            undefined
    end.


safe_binary_to_integer(Version) ->
    try binary_to_integer(Version) of
        Number -> Number
    catch
        _:_ ->
            0
    end.



-define(TYPE_MAPPING, [
                       {<<"oidcp">>, oidcp},
                       {<<"info">>, info},
                       {<<"logout">>, logout},
                       {<<"service">>, service},
                       {<<"credential">>, credential},
                       {<<"access_token">>, access_token},
                       {<<"credential_data">>, cred_data }
                      ]).

id_to_url(Id, _IssuerId, 1) ->
    ApiBase = watts_http_util:relative_path("api"),
    Path = << <<"/v1/credential_data/">>/binary, Id/binary >>,
    << ApiBase/binary, Path/binary>>;
id_to_url(Id, IssuerId, ApiVersion) ->
    ApiBase = watts_http_util:relative_path("api"),
    Version = list_to_binary(io_lib:format("v~p", [ApiVersion])),
    PathElements =[Version, IssuerId, <<"credential_data">>, Id],
    Concat = fun(Element, Path) ->
                     Sep = <<"/">>,
                     << Path/binary, Sep/binary, Element/binary >>
             end,
    Path = lists:foldl(Concat, <<>>, PathElements),
    << ApiBase/binary, Path/binary>>.


verify_type(Type) ->
    case lists:keyfind(Type, 1, ?TYPE_MAPPING) of
        false -> undefined;
        {Type, AtomType} -> AtomType
    end.

verify_id(Id) ->
    Id.

is_malformed(get, _, oidcp, undefined, _, undefined) ->
    false;
is_malformed(get, _, info, undefined, _, undefined) ->
    false;
is_malformed(get, _, logout, undefined, _, undefined) ->
    false;
is_malformed(get, _, service, undefined, Iss, undefined)
  when is_binary(Iss)  ->
    false;
is_malformed(get, _, credential, undefined, Iss, undefined)
  when is_binary(Iss) ->
    false;
is_malformed(get, _, access_token, undefined, _, undefined) ->
    false;
is_malformed(get, _, cred_data, Id, Iss, undefined)
  when is_binary(Iss) ->
    not is_binary(Id);
is_malformed(post, json, credential, undefined, Iss, #{service_id:=Id})
  when is_binary(Iss) ->
    not is_binary(Id);
is_malformed(delete, _, credential, Id, Iss, undefined)
  when is_binary(Iss) ->
    not is_binary(Id);
is_malformed(_, _, _, _, _, _) ->
    true.

is_bad_version(1, true) ->
    false;
is_bad_version(_, true) ->
    true;
is_bad_version(Version, false) when is_integer(Version) ->
   (Version =< 0) or (Version > ?LATEST_VERSION);
is_bad_version(_, _) ->
    true.

end_session_if_rest(#state{session_pid = Session, cookie_based = false}) ->
    perform_logout(Session);
end_session_if_rest(_) ->
    ok.

update_cookie_if_used(Req, #state{cookie_based = true, type=logout})->
    watts_http_util:perform_cookie_action(clear, 0, deleted, Req);
update_cookie_if_used(Req, #state{cookie_based = true, session_pid=Session}) ->
    case watts_session:is_logged_in(Session) of
        true ->
            {ok, Max} = watts_session:get_max_age(Session),
            {ok, Token} = watts_session:get_sess_token(Session),
            watts_http_util:perform_cookie_action(update, Max, Token, Req);
        _ ->
            perform_logout(Session),
            watts_http_util:perform_cookie_action(clear, 0, deleted, Req)
    end;
update_cookie_if_used(Req, #state{cookie_based = _}) ->
    {ok, Req}.


perform_logout(Session) ->
    watts:logout(Session).
