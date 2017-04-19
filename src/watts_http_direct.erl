-module(watts_http_direct).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).


init(_Type, Req, _Opts) ->
    {ok, Req, []}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, _State) ->
    {Path, Req2} = cowboy_req:path(Req),
    JwtData = lists:last(binary:split(Path, <<"/">>, [global, trim_all])),
    %% referer header?
    Jwt = validate_jwt(JwtData),
    setup_session(Jwt, Req2).


validate_jwt(JwtData) ->
    Jwt = erljwt:pre_parse_jwt(JwtData),
    validate_jwt(Jwt, JwtData).
validate_jwt(#{ claims := #{iss := _Iss,
                             sub := _Sub,
                             exp := _Exp,
                             iat := _Iat,
                             watts_service := _Service
                            }} = Jwt, _JwtData) ->
    %TODO:
    % lookup client by issuer
    %% PubKey = get_pubkey_of(Iss),
    % validate JWT
    %% Jwt = erljwt:parse_jwt(JwtData, PubKey, <<"JWT">>),
    Jwt;
validate_jwt(_, _) ->
    invalid.


setup_session(#{ claims := Claims }, Req) ->
    ParameterSet = parameter_set(Claims),
    execute_or_error(ParameterSet, Req);
setup_session( _, Req) ->
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.


execute_or_error(#{ session := SessPid, iss := Iss, sub:= Sub,
                    provider := undefined}, Req) ->
    %% todo:
    %% execute the service
    ok = watts_session:set_iss_sub(Iss, Sub, SessPid),
    {ok, Req, []};
execute_or_error(#{session := SessPid, provider := Provider}, Req) ->
    Path = io_lib:format("oidc?provider=~s", [binary_to_list(Provider)]),
    Url = watts_http_util:relative_path(Path),
    {ok, Max} = watts_session:get_max_age(SessPid),
    {ok, Token} = watts_session:get_sess_token(SessPid),
    {ok, Req2} = watts_http_util:perform_cookie_action(update, Max, Token, Req),
    watts_http_util:redirect_to(Url, Req2);
execute_or_error(_, Req) ->
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.


parameter_set(Claims) ->
    Issuer = maps:get(iss, Claims),
    ServiceId = get_service_id(Claims),
    Params = get_params(Claims),
    Provider = get_provider(Claims),
    {ok, Session} = watts:session_for_direct(ServiceId, Params, Provider,
                                             Issuer),
    #{iss => Issuer, service_id => ServiceId, params => Params,
      provider => Provider, session => Session}.


get_service_id(Claims) ->
    maps:get(watts_service, Claims).

get_params(Claims) ->
    ParamsText = maps:get(watts_params, Claims, undefined),
    ensure_map(safe_decode(ParamsText, #{})).

ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(_) ->
    #{}.


get_provider(Claims) ->
    maps:get(watts_provider, Claims, undefined).



safe_decode(Data, Default) when is_binary(Data) ->
    Res = jsone:try_decode(Data, [{object_format, map}, {keys, attempt_atom}]),
    safe_decode(Res, Default);
safe_decode({ok, Json, _}, _) ->
    Json;
safe_decode(_, Default) ->
    Default.
