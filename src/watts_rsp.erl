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
          return_url = undefined,
          use_referer = false,

          session = #watts_rsp_session{}
         }).



new(#{id := Id, key_location := Location, disable_ui := DisableUi,
      disable_login := DisableLogin, return_url := ReturnUrl,
      use_referer := UseReferer}) ->
    case get_rsp_keys(Location) of
        {ok, Keys} ->

            {ok,  #watts_rsp{id = Id, key_location = Location,
                             keys = Keys, disable_login = DisableLogin,
                             disable_ui = DisableUi, return_url = ReturnUrl,
                             use_referer = UseReferer
                            }};
        {error, Reason} ->
            {error, Reason}
    end.


get_provider(#watts_rsp{session = #watts_rsp_session{ provider = P }}) ->
    P.

get_iss_sub(#watts_rsp{session = #watts_rsp_session{ iss = I, sub = S }}) ->
    Issuer = << <<"rsp-">>/binary, I/binary >>,
    {Issuer, S}.

get_return_url(#watts_rsp{ use_referer = true,
                           session = #watts_rsp_session{ referer = R }}) ->
    R;
get_return_url(#watts_rsp{ use_referer = false, return_url = Url}) ->
    Url.

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


session_type(#watts_rsp{ disable_ui = true} ) ->
    rsp;
session_type(_) ->
    rsp_ui.

request_type(Rsp) ->
    ValidReturn = has_valid_return(Rsp),
    request_type(ValidReturn, Rsp).

request_type(true, #watts_rsp{disable_ui = false, disable_login = false,
                              session = #watts_rsp_session{provider = Provider}
                             }) when is_binary(Provider) ->
    rsp_with_ui_with_login;
request_type(true, #watts_rsp{disable_ui = true, disable_login = false,
                              session = #watts_rsp_session{provider = Provider}
                             }) when is_binary(Provider) ->
    rsp_no_ui_with_login;
request_type(true, #watts_rsp{disable_ui = false, disable_login = true,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->

    rsp_with_ui_no_login;
request_type(true, #watts_rsp{disable_ui = true, disable_login = true,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->
    rsp_no_ui_no_login;
request_type(true, _) ->
    {error, bad_jwt_config};
request_type(false, _) ->
    {error, bad_jwt_return}.



has_valid_return(#watts_rsp{use_referer = UseReferer, return_url = ReturnUrl,
                            session = #watts_rsp_session{ referer = Ref }}) ->
    ValidRefer = UseReferer and is_url(Ref),
    ValidReturn = (not UseReferer) and is_url(ReturnUrl),
    ValidRefer or ValidReturn.

is_url(<< Https:8/binary, _Rest/binary>>) when Https == <<"https://">> ->
    true;
is_url(<< Http:7/binary, _Rest/binary>>) when Http == <<"http://">> ->
    true;
is_url(Url) when is_list(Url) ->
    is_url(list_to_binary(Url));
is_url(_) ->
    false.

update_rsp_on_success({#{claims := Claims }, Rsp}, Referer) ->
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
                 referer = Referer
                },
    {ok, Rsp#watts_rsp{session = Session}};
update_rsp_on_success(_, _) ->
    {error, bad_jwt}.





validate_jwt(JwtData) ->
    Jwt = erljwt:pre_parse_jwt(JwtData),
    validate_jwt(Jwt, JwtData).

validate_jwt(#{ claims := #{iss := Iss,
                             exp := _Exp,
                             iat := _Iat,
                             watts_service := _Service
                            },
               header := #{ kid := _KeyId}
              } = Jwt, _JwtData) ->
    Rsp = get_info(Iss),
%%     Keys = Rsp#watts_rsp.keys,
%%     FilterKey = fun(#{kid := Kid}) ->
%%                         Kid == KeyId
%%                 end,
%%     case lists:filter(FilterKey, Keys) of
%%         [] ->
%%             no_key;
%%         [Key] ->
%%             {erljwt:parse_jwt(JwtData, Key, <<"JWT">>), Rsp};
%%         _ ->
%%             too_many_keys
%%     end;
    {Jwt, Rsp};
validate_jwt(_, _) ->
    invalid.

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

jwt_get_provider(Claims) ->
    maps:get(watts_provider, Claims, undefined).

jwt_get_subject(Claims) ->
    maps:get(sub, Claims, undefined).

jwt_get_issuer(Claims) ->
    maps:get(iss, Claims).

jwt_get_service_id(Claims) ->
    maps:get(watts_service, Claims).

jwt_get_params(Claims) ->
    ParamsText = maps:get(watts_params, Claims, undefined),
    ensure_map(safe_decode(ParamsText, #{})).

ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_) ->
    #{}.
