-module(watts_http_rsp).
-behaviour(cowboy_http_handler).
-export([
         init/3,
         terminate/3,
         handle/2]).

-include("watts.hrl").

init(_Type, Req, _Opts) ->
    {ok, Req, []}.

terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, _State) ->
    {Path, Req2} = cowboy_req:path(Req),
    {Referer, Req3} = cowboy_req:header(<<"referer">>, Req2),
    JwtData = lists:last(binary:split(Path, <<"/">>, [global, trim_all])),
    Jwt = validate_jwt(JwtData),
    JwtRef = maps:merge(Jwt, #{referer => Referer}),
    setup_session(JwtRef, Req3).


validate_jwt(JwtData) ->
    Jwt = erljwt:pre_parse_jwt(JwtData),
    validate_jwt(Jwt, JwtData).
validate_jwt(#{ claims := #{iss := Iss,
                             exp := _Exp,
                             iat := _Iat,
                             watts_service := _Service
                            },
               header := #{ kid := KeyId}
              }, JwtData) ->
    Keys = maps:get(keys, get_rsp_info(Iss), []),
    FilterKey = fun(#{kid := Kid}) ->
                        Kid == KeyId
                end,
    case lists:filter(FilterKey, Keys) of
        [] ->
            no_key;
        [Key] ->
            erljwt:parse_jwt(JwtData, Key, <<"JWT">>);
        _ ->
            too_many_keys
    end;
validate_jwt(_, _) ->
    invalid.


setup_session(#{ claims := Claims, referer := Referer }, Req) ->
    ParameterSet = parameter_set(Claims, Referer),
    execute_or_error(ParameterSet, Req);
setup_session(Error, Req) ->
    lager:warning("RST: failed due to ~p", [Error]),
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.


execute_or_error(#{sub := undefined, provider := undefined} = Map, Req) ->
    bad_request(Map, Req);
execute_or_error(#{ session := SessPid, iss := Iss, sub:= Sub,
                    rsp_info := #{disable_login := true,
                                  disable_ui := _DisableUi}}, Req) ->
    ok = watts_session:set_iss_sub(Iss, Sub, SessPid),
    %% todo:
    %% execute the service either with or without ui
    {ok, Req, []};
execute_or_error(#{session := SessPid, provider := Provider,
                  rsp_info := #{disable_login := false}}, Req)
  when is_binary(Provider) ->
    Path = io_lib:format("oidc?provider=~s", [binary_to_list(Provider)]),
    Url = watts_http_util:relative_path(Path),
    {ok, Max} = watts_session:get_max_age(SessPid),
    {ok, Token} = watts_session:get_sess_token(SessPid),
    {ok, Req2} = watts_http_util:perform_cookie_action(update, Max, Token, Req),
    watts_http_util:redirect_to(Url, Req2);
execute_or_error(Map, Req) ->
    bad_request(Map, Req).

bad_request(#{session := Session}, Req) ->
    watts:logout(Session),
    {ok, Req2} = cowboy_req:reply(400, Req),
    {ok, Req2, []}.



parameter_set(Claims, Referer) ->
    Issuer = get_issuer(Claims),
    Rsp0 = get_rsp_info(Issuer),
    Subject = get_subject(Claims),
    ServId = get_service_id(Claims),
    Params = get_params(Claims),
    Provider = get_provider(Claims),
    Url = case maps:get(return_to_referer, Rsp0) of
              true -> Referer;
              false -> maps:get(return_url, Rsp0)
          end,
    Rsp = maps:merge(Rsp0, #{referer => Referer, url => Url}),
    {ok, Session} = start_rsp_session(ServId, Params, Provider, Rsp),
    #{iss => Issuer, sub => Subject, service_id => ServId, params => Params,
      provider => Provider, session => Session, rsp_info => Rsp}.

start_rsp_session(ServiceId, Params, Provider, #{disable_ui := true} = Rsp) ->
    watts:session_for_direct(ServiceId, Params, Provider, Rsp);
start_rsp_session(ServiceId, Params, Provider, Rsp) ->
    watts:session_for_direct_ui(ServiceId, Params, Provider, Rsp).


get_subject(Claims) ->
    maps:get(sub, Claims, undefined).

get_issuer(Claims) ->
    maps:get(iss, Claims).

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

get_rsp_info(Id) ->
    FindId = fun(#{id := I}) ->
                     I == Id
             end,
    case lists:filter(FindId, ?CONFIG(rsp_list, [])) of
        [Info] -> Info;
        _ -> #{}
    end.
