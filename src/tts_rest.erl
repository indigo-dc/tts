-module(tts_rest).
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
                       Len = binary:byte_size(InBasePath),
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
          object = undefined,
          provider = undefined,
          json = undefined,
          user_info = undefined
         }).

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.


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
    {InIssuer, Req5} = cowboy_req:header(<<"x-openid-connect-issuer">>, Req4),
    {Method, Req6} = cowboy_req:method(Req5),
    {ok, InBody, Req7} = cowboy_req:body(Req6),
    {Result, NewState} = is_malformed(Method, InVersion, InType, InId,
                                      InBody, InToken, InIssuer, State),
    {Result, Req7, NewState}.


is_authorized(Req, #state{type=oidcp} = State) ->
    {true, Req, State};
is_authorized(Req, #state{token=undefined} = State) ->
    {{false, <<"Authorization">>}, Req, State};
is_authorized(Req, #state{type=Type, token=Token} = State)
  when Type==service; Type==credential; Type==cred_data ->
    {ok, [{ProviderId, _}| _]} = oidcc:get_openid_provider_list(),
    {ok, Info} = oidcc:get_openid_provider_info(ProviderId),
    #{issuer := Issuer} = Info,
    case oidcc:retrieve_user_info(Token, ProviderId) of
        {ok, #{sub := Subject}} ->
            case tts_user_cache:get_user_info(Issuer, Subject) of
                {ok, UserInfo} ->
                    {true, Req, State#state{user_info=UserInfo}};
                _ -> {{false, <<"Authorization">>}, Req, State}
            end;
        _ -> {{false, <<"Authorization">>}, Req, State}
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
resource_exists(Req, #state{type=oidcp, id=Id} = State) ->
    case oidcc:get_openid_provider_info(Id) of
        {ok, Info} ->
            {true, Req, State#state{object = Info}};
        _ -> {false, Req, State}
    end;
resource_exists(Req, #state{type=service, id=Id} = State) ->
    case tts_service:get_info(Id) of
        {ok, Info} ->
            {true, Req, State#state{object = Info}};
        _ -> {false, Req, State}
    end;
resource_exists(Req, #state{type=credential, id=Id,
                            user_info=#{uid :=UserId}}=State) ->
    Result = tts_credential:exists(UserId, Id),
    {Result, Req, State};
resource_exists(Req, #state{type=cred_data}=State) ->
    {true, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

delete_resource(Req, #state{type=credential,
                            id=CredentialId, user_info=UserInfo}=State) ->
     case tts_credential:revoke(CredentialId, UserInfo) of
         {ok, _, _} -> {true, Req, State};
         _ -> {false, Req, State}
     end.


get_json(Req, #state{version=Version, type=Type, id=Id, method=get,
                     object=Object, user_info=UserInfo } = State) ->
    Result = perform_get(Type, Id, Object, UserInfo, Version),
    {Result, Req, State}.


post_json(Req, #state{version=Version, type=Type, id=Id, method=post,
                      user_info=UserInfo, json=Json} = State) ->
    Result = perform_post(Type, Id, Json, UserInfo, Version),
    {Result, Req, State}.

perform_get(service, undefined, _, #{uid:=UserId}, _Version) ->
    {ok, ServiceList} = tts_service:get_list(UserId),
    return_service_list(ServiceList);
perform_get(oidcp, undefined, _, _, _Version) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    return_oidc_list(OIDCList);
perform_get(credential, undefined, _, #{uid := UserId}, _Version) ->
    {ok, CredList} = tts_credential:get_list(UserId),
    return_credential_list(CredList);
perform_get(cred_data, Id, _, #{uid := UserId}, _Version) ->
    case tts_rest_cred:get_cred(Id, UserId) of
        {ok, Cred} -> jsx:encode(Cred);
        _ -> jsx:encode(#{})
    end.

perform_post(credential, undefined, #{service_id:=ServiceId}, UserInfo, Ver) ->
    IFace = <<"REST interface">>,
    case  tts_credential:request(ServiceId, UserInfo, IFace, rest, []) of
        {ok, Credential, _Log} ->
            #{ uid := UserId } = UserInfo,
            {ok, Id} = tts_rest_cred:add_cred(Credential, UserId),
            Url = id_to_url(Id, Ver),
            {true, Url};
        _ ->
            false
    end.

return_service_list(Services) ->
    Extract = fun(Map, List) ->
                      Keys = [id, type, host, port],
                      [ maps:with(Keys, Map) | List]
              end,
    List = lists:reverse(lists:foldl(Extract, [], Services)),
    jsx:encode(#{service_list => List}).

return_oidc_list(Oidc) ->
    Id = fun({OidcId, Pid}, List) ->
                 {ok, #{issuer := Issuer}} =
                 oidcc:get_openid_provider_info(Pid),
                 [#{ id => OidcId, issuer => Issuer} | List]
         end,
    List = lists:reverse(lists:foldl(Id, [], Oidc)),
    jsx:encode(#{openid_provider_list => List}).

return_credential_list(Credentials) ->
    Id = fun(CredId, List) ->
                 [#{ id => CredId} | List]
         end,
    List = lists:reverse(lists:foldl(Id, [], Credentials)),
    jsx:encode(#{credential_list => List}).


is_malformed(InMethod, InVersion, InType, InId, InBody, InToken, InIssuer,
             State) ->
    Version = verify_version(InVersion),
    Type = verify_type(InType),
    Id = verify_id(InId),
    Token = verify_token(InToken),
    Issuer = verify_issuer(InIssuer),
    Method = verify_method(InMethod),
    Body = verify_body(InBody),
    case is_bad_version(Version) of
        true -> {true, State#state{method=Method, version=Version, type=Type,
                                   id=Id, token=Token, issuer=Issuer,
                                   json=Body}};
        false -> Result = is_malformed(Method, Type, Id, Body),
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

is_malformed(get, oidcp, undefined, undefined) ->
    false;
is_malformed(get, service, undefined, undefined) ->
    false;
is_malformed(get, credential, undefined, undefined) ->
    false;
is_malformed(get, cred_data, Id, undefined) ->
    not is_binary(Id);
is_malformed(post,  credential, undefined, #{service_id:=_Id}) ->
    false;
is_malformed(delete, credential, Id, undefined) ->
    not is_binary(Id);
is_malformed(_, _, _, _) ->
    true.

is_bad_version(Version) when is_integer(Version) ->
   (Version =< 0) or (Version > ?LATEST_VERSION);
is_bad_version(_) ->
    true.



