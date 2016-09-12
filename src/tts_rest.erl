-module(tts_rest).
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


-export([dispatch_mapping/1]).

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

dispatch_mapping(InBasePath) ->
    BasePath = case binary:last(InBasePath) of
                   $/ ->
                       Len = byte_size(InBasePath),
                       binary:part(InBasePath, 0, Len-1);
                   _ ->
                       InBasePath
               end,
    << BasePath/binary, <<"/[:version]/:type/[:id]">>/binary >>.

%%
%% REST implementation
%%

-define(LATEST_VERSION, 1).

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
          session_pid = undefined
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
    {InVersion, Req2} = cowboy_req:binding(version, Req, latest),
    {InType, Req3} = cowboy_req:binding(type, Req2),
    {InId, Req4} = cowboy_req:binding(id, Req3, undefined),
    {InToken, Req5} = cowboy_req:header(<<"authorization">>, Req4),
    {InIssuer, Req6} = cowboy_req:header(<<"x-openid-connect-issuer">>,
                                         Req5),
    {Res, ContentType, Req7} = cowboy_req:parse_header(<<"content-type">>,
                                                       Req6),
    {Method, Req8} = cowboy_req:method(Req7),
    {ok, InBody, Req9} = cowboy_req:body(Req8),

    {Result, NewState} = is_malformed(Method, {Res, ContentType}, InVersion,
                                      InType , InId, InBody, InToken,
                                      InIssuer , State),
    {Result, Req9, NewState}.


is_authorized(Req, #state{type=oidcp} = State) ->
    {true, Req, State};
is_authorized(Req, #state{type=Type, token=Token, issuer=Issuer} = State)
  when Type==service; Type==credential; Type==cred_data ->
    case tts:login_with_access_token(Token, Issuer) of
        {ok, #{session_pid := SessionPid}} ->
            {true, Req, State#state{session_pid = SessionPid}};
        {error, _} ->
            {{false, <<"Authorization">>}, Req, State}

    end;
is_authorized(Req, State) ->
    {{false, <<"Authorization">>}, Req, State}.

content_types_provided(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, get_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {{<<"application">>, <<"json">>, '*'}, post_json }
     ], Req, State}.

resource_exists(Req, #state{id=undefined} = State) ->
    {true, Req, State};
resource_exists(Req, #state{type=credential, id=Id, session_pid=Session}
                = State) ->
    Exists = tts:does_credential_exist(Id, Session),
    {Exists, Req, State};
resource_exists(Req, #state{type=cred_data, id=Id, session_pid=Session}
                = State) ->
    Exists = tts:does_temp_cred_exist(Id, Session),
    {Exists, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

delete_resource(Req, #state{type=credential,
                            id=CredentialId, session_pid=Session}=State) ->
    Result = case tts:revoke_credential_for(CredentialId, Session) of
                 {ok, _, _} -> true;
                 _ -> false
             end,
    ok = tts:logout(Session),
    {Result, Req, State#state{session_pid=undefined}}.


get_json(Req, #state{version=Version, type=Type, id=Id, method=get,
                     session_pid=Session } = State) ->
    Result = perform_get(Type, Id, Session, Version),
    ok = tts:logout(Session),
    {Result, Req, State#state{session_pid=undefined}}.


post_json(Req, #state{version=Version, type=Type, id=Id, method=post,
                      session_pid=Session, json=Json} = State) ->
    Result = perform_post(Type, Id, Json, Session, Version),
    ok = tts:logout(Session),
    {Result, Req, State#state{session_pid=undefined}}.

perform_get(service, undefined, Session, _Version) ->
    {ok, ServiceList} = tts:get_service_list_for(Session),
    return_json_service_list(ServiceList);
perform_get(oidcp, undefined, _, _Version) ->
    {ok, OIDCList} = tts:get_openid_provider_list(),
    return_json_oidc_list(OIDCList);
perform_get(credential, undefined, Session, _Version) ->
    {ok, CredList} = tts:get_credential_list_for(Session),
    return_json_credential_list(CredList);
perform_get(cred_data, Id, Session, _Version) ->
    case tts:get_temp_cred(Id, Session) of
        {ok, Cred} -> jsx:encode(Cred);
        _ -> jsx:encode(#{})
    end.

perform_post(credential, undefined, #{service_id:=ServiceId}, Session, Ver) ->
    IFace = <<"REST interface">>,
    case  tts:request_credential_for(ServiceId, Session, [], IFace) of
        {ok, Credential, _Log} ->
            {ok, Id} = tts:store_temp_cred(Credential, Session),
            Url = id_to_url(Id, Ver),
            {true, Url};
        _ ->
            false
    end.

return_json_service_list(Services) ->
    Extract = fun(Map, List) ->
                      Keys = [id, type, host, port],
                      [ maps:with(Keys, Map) | List]
              end,
    List = lists:reverse(lists:foldl(Extract, [], Services)),
    jsx:encode(#{service_list => List}).

return_json_oidc_list(Oidc) ->
    Id = fun(OidcInfo, List) ->
                 case OidcInfo of
                 #{issuer := Issuer, id := Id, ready := true}  ->
                         [#{ id => Id, issuer => Issuer} | List];
                     _ -> List
                 end
         end,
    List = lists:reverse(lists:foldl(Id, [], Oidc)),
    jsx:encode(#{openid_provider_list => List}).

return_json_credential_list(Credentials) ->
    Id = fun(CredId, List) ->
                 [#{ id => CredId} | List]
         end,
    List = lists:reverse(lists:foldl(Id, [], Credentials)),
    jsx:encode(#{credential_list => List}).


is_malformed(InMethod, InContentType, InVersion, InType, InId, InBody, InToken,
             InIssuer, State) ->
    Version = verify_version(InVersion),
    Type = verify_type(InType),
    Id = verify_id(InId),
    Token = verify_token(InToken),
    Issuer = verify_issuer(InIssuer),
    Method = verify_method(InMethod),
    ContentType = verify_content_type(InContentType),
    Body = verify_body(InBody),
    case is_bad_version(Version) of
        true -> {true, State#state{method=Method, version=Version, type=Type,
                                   id=Id, token=Token, issuer=Issuer,
                                   json=Body}};
        false -> Result = is_malformed(Method, ContentType, Type, Id, Body),
                 {Result, State#state{method=Method, version=Version, type=Type,
                                      id=Id, token=Token, json=Body,
                                      issuer=Issuer}}
    end.

verify_version(latest) ->
    ?LATEST_VERSION;
verify_version(<<"latest">>) ->
    verify_version(latest);
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
verify_content_type({undefined, _}) ->
    undefined;
verify_content_type(_) ->
    unsupported.


verify_issuer(undefined) ->
    undefined;
verify_issuer(Issuer) when is_binary(Issuer) ->
    case oidcc:get_openid_provider_info(Issuer) of
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

verify_method(<<"GET">>) ->
    get;
verify_method(<<"POST">>) ->
    post;
verify_method(<<"DELETE">>) ->
    delete.

verify_body([]) ->
    undefined;
verify_body(Data) ->
    case jsx:is_json(Data) of
        true ->
            jsx:decode(Data, [{labels, attempt_atom}, return_maps]);
        false ->
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
                       {<<"service">>, service},
                       {<<"oidcp">>, oidcp},
                       {<<"credential">>, credential},
                       {<<"credential_data">>, cred_data }
                      ]).

id_to_url(Id, CurrentVersion) ->
    Base = ?CONFIG(ep_api),
    Version = list_to_binary(io_lib:format("v~p", [CurrentVersion])),
    PathElements =[Version, <<"credential_data">>, Id],
    Concat = fun(Element, Path) ->
                     Sep = <<"/">>,
                     << Path/binary, Sep/binary, Element/binary >>
             end,
    Path = lists:foldl(Concat, <<>>, PathElements),
    << Base/binary, Path/binary>>.


verify_type(Type) ->
    case lists:keyfind(Type, 1, ?TYPE_MAPPING) of
        false -> undefined;
        {Type, AtomType} -> AtomType
    end.

verify_id(Id) ->
    Id.

is_malformed(get, _, oidcp, undefined, undefined) ->
    false;
is_malformed(get, _, service, undefined, undefined) ->
    false;
is_malformed(get, _, credential, undefined, undefined) ->
    false;
is_malformed(get, _, cred_data, Id, undefined) ->
    not is_binary(Id);
is_malformed(post, json, credential, undefined, #{service_id:=_Id}) ->
    false;
is_malformed(delete, _, credential, Id, undefined) ->
    not is_binary(Id);
is_malformed(_, _, _, _, _) ->
    true.

is_bad_version(Version) when is_integer(Version) ->
   (Version =< 0) or (Version > ?LATEST_VERSION);
is_bad_version(_) ->
    true.
