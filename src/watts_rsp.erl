-module(watts_rsp).
-include("watts.hrl").

-export([new/1,
         get_info/1,
         validate_jwt_get_rsp/2,
         request_type/1,
         session_type/1,
         get_provider/1,
         get_iss_sub/1,
         get_return_url/1,
         get_service_data/1
        ]).


-record(watts_rsp_session, {
          referer = undefined,
          success_url = undefined,
          failed_url = undefined,
          error_url = undefined,
          iss = undefined,
          sub = undefined,
          service_id = undefined,
          params = undefined,
          provider = undefined
         }).

-record(watts_rsp, {
          id = undefined,
          key_location = undefined,
          keys = [],
          disable_login = false,
          disable_ui = false,
          base_url = undefined,

          session = #watts_rsp_session{}
         }).



new(#{id := Id, key_location := Location, disable_ui := DisableUi,
      disable_login := DisableLogin, base_url := BaseUrl}) ->
    case get_rsp_keys(Location) of
        {ok, Keys} ->
            {ok,  #watts_rsp{id = Id, key_location = Location,
                             keys = Keys, disable_login = DisableLogin,
                             disable_ui = DisableUi, base_url = BaseUrl
                            }};
        {error, Reason} ->
            {error, Reason}
    end.


get_provider(#watts_rsp{session = #watts_rsp_session{ provider = P }}) ->
    P.

get_iss_sub(#watts_rsp{session = #watts_rsp_session{ iss = I, sub = S }}) ->
    Issuer = << <<"rsp-">>/binary, I/binary >>,
    {Issuer, S}.

get_return_url(#watts_rsp{session = Session}) ->
    session_return_url(Session).

session_return_url(#watts_rsp_session{ referer=R, success_url=undefined }) ->
    R;
session_return_url(#watts_rsp_session{ success_url=R }) ->
    R.


get_service_data(#watts_rsp{ session = #watts_rsp_session{ service_id = Id,
                                                           params = Param}}) ->
    {Id, Param}.

get_info(Id) ->
    FindId = fun(#watts_rsp{id = I}) ->
                     I == Id
             end,
    case lists:filter(FindId, ?CONFIG(rsp_list, [])) of
        [Rsp] -> Rsp;
        _ -> #watts_rsp{}
    end.

validate_jwt_get_rsp(JwtData, Referer) ->
    update_rsp_on_success(validate_jwt(JwtData), Referer).


session_type(#watts_rsp{ disable_ui = true, disable_login = true} ) ->
    {rsp, no_ui, no_login};
session_type(#watts_rsp{ disable_ui = true, disable_login = false} ) ->
    {rsp, no_ui, login};
session_type(#watts_rsp{ disable_ui = false, disable_login = true} ) ->
    {rsp, ui, no_login};
session_type(_) ->
    {rsp, ui, login}.

request_type(Rsp) ->
    ValidReturn = has_valid_return(Rsp),
    request_type(ValidReturn, Rsp).

request_type(true, #watts_rsp{disable_ui = false, disable_login = false,
                              session = #watts_rsp_session{provider = Provider,
                                                           sub = Sub}
                             }) when is_binary(Provider), is_binary(Sub) ->
    rsp_ui_login;
request_type(true, #watts_rsp{disable_ui = true, disable_login = false,
                              session = #watts_rsp_session{provider = Provider,
                                                          sub = Sub}
                             }) when is_binary(Provider), is_binary(Sub) ->
    rsp_no_ui_login;
request_type(true, #watts_rsp{disable_ui = false, disable_login = true,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->
    rsp_ui_no_login;
request_type(true, #watts_rsp{disable_ui = true, disable_login = true,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->
    rsp_no_ui_no_login;
request_type(true, _) ->
    {error, bad_jwt_config};
request_type(false, _) ->
    {error, bad_jwt_return}.



has_valid_return(#watts_rsp{base_url = BaseUrl, session = Session}) ->
    UrlList = extract_urls(Session),
    EnsureValidUrls = fun(Url, Current) ->
                              is_valid_url(Url, BaseUrl) and Current
                      end,
    lists:foldl(EnsureValidUrls, true, UrlList).



extract_urls(#watts_rsp_session{referer=R, success_url=S, failed_url=F}) ->
    List = [R, S, F],
    FilterAtom = fun(Url) ->
                         not is_atom(Url)
                 end,
    lists:filter(FilterAtom, List).



is_valid_url(<< Https:8/binary, _Rest/binary>> = Url, Base)
  when Https == <<"https://">> ->
    starts_with_base(Url, Base);
is_valid_url(<< Http:7/binary, _Rest/binary>> = Url, Base)
  when Http == <<"http://">> ->
    starts_with_base(Url, Base);
is_valid_url(Url, Base) when is_list(Url) ->
    is_valid_url(list_to_binary(Url), Base);
is_valid_url(_, _) ->
    false.

starts_with_base(Url, Base) ->
    binary:match(Url, Base) == {0, byte_size(Base)}.


update_rsp_on_success({Claims, Rsp}, Referer) when is_map(Claims) ->
    Success = jwt_get_success_url(Claims),
    Failed = jwt_get_failed_url(Claims),
    Issuer = jwt_get_issuer(Claims),
    Subject = jwt_get_subject(Claims),
    ServiceId = jwt_get_service_id(Claims),
    Params = jwt_get_params(Claims),
    Provider = jwt_get_provider(Claims),
    Session = #watts_rsp_session{
                 iss = Issuer,
                 sub = Subject,
                 service_id = ServiceId,
                 params = Params,
                 provider = Provider,
                 success_url = Success,
                 failed_url = Failed,
                 referer = Referer
                },
    {ok, Rsp#watts_rsp{session = Session}};
update_rsp_on_success({Error, _Rsp}, _) ->
    {error, {bad_jwt, Error}}.





validate_jwt(JwtData) ->
    Jwt = erljwt:pre_parse_jwt(JwtData),
    validate_jwt(Jwt, JwtData).

validate_jwt(#{ claims := #{iss := Iss,
                            sub := _Sub,
                            exp := _Exp,
                            iat := _Iat,
                            watts_service := _Service
                           },
                header := #{ kid := KeyId, alg := <<"RS256">>}
              } , JwtData) ->
    Rsp = get_info(Iss),
    Keys = Rsp#watts_rsp.keys,
    FilterKey = fun(#{kid := Kid}) ->
                        Kid == KeyId;
                   (_) ->
                        false
                end,
    Jwt =   case lists:filter(FilterKey, Keys) of
                [] ->
                    no_key;
                [#{key := Key}] ->
                    erljwt:parse_jwt(JwtData, Key, <<"JWT">>);
                [_] ->
                    bad_key;
                _ ->
                    too_many_keys
            end,
    {Jwt, Rsp};
validate_jwt(_, _) ->
    {invalid, unknown}.

get_rsp_keys(<< File:7/binary, Path/binary >>)
  when File == <<"file://">> ->
    Result = file:read_file(binary_to_list(Path)),
    extract_rsp_keys(Result);
get_rsp_keys(<< Https:8/binary, _Rest/binary >> = Url)
  when Https == <<"https://">> ->
    Request = { binary_to_list(Url), [] },
    HttpOpt = [],
    Options = [{sync, true}, {full_result, true}],
    Result = httpc:request(get, Request, HttpOpt, Options),
    extract_rsp_keys(Result).


extract_rsp_keys({ok, Data}) when is_binary(Data) ->
    Json = safe_decode(Data, bad_data),
    extract_rsp_keys(Json, []);
extract_rsp_keys({ok, Data}) when is_list(Data) ->
    extract_rsp_keys({ok, list_to_binary(Data)});
extract_rsp_keys({ok, {Code, Body}}) when Code >= 200, Code < 300 ->
    extract_rsp_keys({ok, Body});
extract_rsp_keys({ok, {{_, Code, _}, _, Body}}) ->
    extract_rsp_keys({ok, {Code, Body}});
extract_rsp_keys({error, Reason}) ->
    {error, list_to_binary(io_lib:format("error reading keys: ~p", [Reason]))}.

extract_rsp_keys([], List) ->
    {ok, List};
extract_rsp_keys([#{kty := <<"RSA">>,
                    use := <<"sig">>,
                    alg := <<"RS256">>,
                    kid := KeyId,
                    n := N,
                    e := E
                   } | T], List) ->
    extract_rsp_keys(T, [ rsp_rsa_key(KeyId, N, E), List ]);
extract_rsp_keys([H | T], List) ->
    lager:warning("Init: skipping unsupported key ~p", [H]),
    extract_rsp_keys(T, List);
extract_rsp_keys(#{keys := Keys}, List) ->
    extract_rsp_keys(Keys, List);
extract_rsp_keys(_, _List) ->
    {error, bad_json}.

rsp_rsa_key(KeyId, N0, E0) ->
    N = binary:decode_unsigned(base64url:decode(N0)),
    E = binary:decode_unsigned(base64url:decode(E0)),
    Key = [E, N],
    #{kty => rsa, alg => rs256, use => sign, kid => KeyId, key => Key}.

safe_decode(Data, Default) when is_binary(Data) ->
    Res = jsone:try_decode(Data, [{object_format, map}, {keys, attempt_atom}]),
    safe_decode(Res, Default);
safe_decode({ok, Json, _}, _) ->
    Json;
safe_decode(_, Default) ->
    Default.

jwt_get_success_url(Claims) ->
    maps:get(success_url, Claims, undefined).

jwt_get_failed_url(Claims) ->
    maps:get(failed_url, Claims, undefined).

jwt_get_provider(Claims) ->
    maps:get(watts_provider, Claims, undefined).

jwt_get_subject(Claims) ->
    maps:get(sub, Claims, undefined).

jwt_get_issuer(Claims) ->
    maps:get(iss, Claims).

jwt_get_service_id(Claims) ->
    maps:get(watts_service, Claims).

jwt_get_params(Claims) ->
    Params = maps:get(watts_params, Claims, #{}),
    ensure_map(Params).

ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(MapList) when is_list(MapList) ->
    maps:from_list(MapList);
ensure_map(_) ->
    #{}.
