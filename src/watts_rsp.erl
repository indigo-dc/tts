%% @doc this module implements the RSP interface of WaTTS.
%% This includes checking if an RSP is configured, validating the passed jwts
%% and sending users back to the RSPs.
-module(watts_rsp).
-include("watts.hrl").
-include_lib("public_key/include/public_key.hrl").


-export([new/1,
         exists/1,
         get_info/1,
         get_id/1,
         validate_jwt_get_rsp/2,
         request_type/1,
         session_type/1,
         get_provider/1,
         get_iss_sub/1,
         get_return_urls/1,
         get_service_data/1,
         get_list/0
        ]).

-export_type([rsp/0, request_type/0]).

-record(watts_rsp_session, {
          referer = undefined :: undefined | binary(),
          success_url = undefined :: undefined | binary(),
          failed_url = undefined :: undefined | binary(),
          iss = undefined :: undefined | binary(),
          sub = undefined :: undefined | binary(),
          service_id = undefined :: undefined | binary(),
          params = undefined :: undefined | map(),
          provider = undefined :: undefined | binary()
         }).

-record(watts_rsp, {
          id = undefined :: undefined | binary(),
          key_location = undefined :: undefined | binary(),
          key_pid = undefined :: undefined | pid(),
          perform_login = true :: boolean(),
          show_ui = true :: boolean(),
          base_url = undefined :: undefined | binary(),

          session = #watts_rsp_session{} :: session()
         }).

-type rsp() :: #watts_rsp{}.
-type session() :: #watts_rsp_session{}.
-type request_type() :: rsp_ui_login | rsp_no_ui_login | rsp_ui_no_login |
                        rsp_no_ui_no_login .

%% @doc create a new rsp configuration from the data in the config file.
%% It also starts a {@link watts_rsp_keys} process and referces it.
-spec new(map()) -> {ok, rsp()}.
new(#{id := Id, key_location := Location, show_ui := Ui,
      perform_login := Login, base_url := BaseUrl} = Map) ->
    {ok, Pid} = watts_rsp_keys:new(Map),
    {ok,  #watts_rsp{id = Id, key_location = Location,
                     key_pid = Pid, perform_login = Login,
                     show_ui = Ui, base_url = BaseUrl
                    }}.


%% @doc get the list of all configured RSPs
-spec get_list() -> {ok, [rsp()]}.
get_list() ->
    {ok, ?CONFIG(rsp_list)}.

%% @doc get the rsp config from the list of configs, if it exists
-spec get_info(binary()) -> rsp().
get_info(Id) ->
    FindId = fun(#watts_rsp{id = I}) ->
                     I == Id
             end,
    case lists:filter(FindId, ?CONFIG(rsp_list, [])) of
        [Rsp] -> Rsp;
        _ -> #watts_rsp{}
    end.

%% @doc check if an rsp exists
-spec exists(binary()) -> boolean().
exists(RspId) ->
    case get_info(RspId) of
        #watts_rsp{id = undefined} ->
            false;
        _ ->
            true
    end.

%% @doc return the id of an rsp, given its configuration
-spec get_id(rsp()) -> {ok, binary()}.
get_id(#watts_rsp{ id = Id }) ->
    {ok, Id}.

%% @doc return the provider id of an rsp, given its configuration (session)
-spec get_provider(rsp()) -> binary() | undefined.
get_provider(#watts_rsp{session = #watts_rsp_session{ provider = P }}) ->
    P.


%% @doc return the iss/sub of an rsp, given its configuration (session)
-spec get_iss_sub(rsp()) -> {binary(), binary()}.
get_iss_sub(#watts_rsp{id = I, session = #watts_rsp_session{ sub = S }}) ->
    Issuer = << <<"rsp-">>/binary, I/binary >>,
    {Issuer, S}.


%% @doc return the return urls of an rsp, given its configuration (session)
-spec get_return_urls(rsp()) -> {binary(), binary()}.
get_return_urls(#watts_rsp{session = Session}) ->
    session_return_urls(Session).


%% @doc return the success and failure url from a rsp session.
-spec session_return_urls(session()) -> {binary(), binary()}.
session_return_urls(#watts_rsp_session{ referer=R, success_url=undefined,
                                       failed_url=undefined }) ->
    {R, R};
session_return_urls(#watts_rsp_session{ success_url=Succ,
                                        failed_url=undefined }) ->
    {Succ, Succ};
session_return_urls(#watts_rsp_session{ success_url=Succ,
                                        failed_url=Err }) ->
    {Succ, Err}.



%% @doc return the service id and params from the rsp configuration (session).
-spec get_service_data(rsp()) -> {binary(), map()}.
get_service_data(#watts_rsp{ session = #watts_rsp_session{ service_id = Id,
                                                           params = Param}}) ->
    {Id, Param}.



%% @doc return the session type of the rsp.
%% this is basically a tuple indicating that it is an rsp session
%% and wether the ui should be shown and if the login should be
%% performed or not.
-spec session_type(rsp()) -> watts_session:type().
session_type(#watts_rsp{ show_ui = false, perform_login = false} ) ->
    {rsp, no_ui, no_login};
session_type(#watts_rsp{ show_ui = false, perform_login = true} ) ->
    {rsp, no_ui, login};
session_type(#watts_rsp{ show_ui = true, perform_login = false} ) ->
    {rsp, ui, no_login};
session_type(_) ->
    {rsp, ui, login}.


%% @doc return the request type, an atom, of the rsp, if valid.
%% Ensure the request has valid return urls
-spec request_type(rsp()) -> request_type() | {error, Reason :: atom()}.
request_type(Rsp) ->
    ValidReturn = has_valid_return(Rsp),
    request_type(ValidReturn, Rsp).

%% @doc return the request type, an atom, of the rsp, if valid.
-spec request_type(boolean(), rsp()) -> rsp_ui_login | rsp_no_ui_login |
                                        rsp_ui_no_login | rsp_no_ui_no_login |
                                        {error, Reason :: atom()}.
request_type(true, #watts_rsp{show_ui = true, perform_login = true,
                              session = #watts_rsp_session{provider = Provider,
                                                           sub = Sub}
                             }) when is_binary(Provider), is_binary(Sub) ->
    rsp_ui_login;
request_type(true, #watts_rsp{show_ui = false, perform_login = true,
                              session = #watts_rsp_session{provider = Provider,
                                                          sub = Sub}
                             }) when is_binary(Provider), is_binary(Sub) ->
    rsp_no_ui_login;
request_type(true, #watts_rsp{show_ui = true, perform_login = false,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->
    rsp_ui_no_login;
request_type(true, #watts_rsp{show_ui = false, perform_login = false,
                              session = #watts_rsp_session{sub = Sub}
                             }) when is_binary(Sub) ->
    rsp_no_ui_no_login;
request_type(true, _) ->
    {error, bad_jwt_config};
request_type(false, _) ->
    {error, bad_jwt_return}.


%% @doc check if the rsp has valid return urls (also session).
-spec has_valid_return(rsp()) -> boolean().
has_valid_return(#watts_rsp{base_url = BaseUrl, session = Session}) ->
    UrlList = extract_urls(Session),
    EnsureValidUrls = fun(Url, Current) ->
                              is_valid_url(Url, BaseUrl) and Current
                      end,
    lists:foldl(EnsureValidUrls, true, UrlList).


%% @doc get the urls from the session (passed in via the jwt)
-spec extract_urls(session()) -> [binary()].
extract_urls(#watts_rsp_session{referer=R, success_url=S, failed_url=F}) ->
    List = [R, S, F],
    FilterAtom = fun(Url) ->
                         is_binary(Url)
                 end,
    lists:filter(FilterAtom, List).

%% @doc check if a url is valid (needs to start with http(s) and have the
%% configured base.
-spec is_valid_url(binary(), binary()) -> boolean().
is_valid_url(<< Https:8/binary, _Rest/binary>> = Url, Base)
  when Https == <<"https://">> ->
    starts_with_base(Url, Base);
is_valid_url(<< Http:7/binary, _Rest/binary>> = Url, Base)
  when Http == <<"http://">> ->
    starts_with_base(Url, Base);
is_valid_url(_, _) ->
    false.

%% @doc check if the url starts with the configured base url
-spec starts_with_base(binary(), binary()) -> boolean().
starts_with_base(Url, Base) ->
    binary:match(Url, Base) == {0, byte_size(Base)}.


%% @doc validate the passed jwt data with the referer.
-spec validate_jwt_get_rsp(binary(), binary())
                          -> {ok, rsp()} |
                             {error, Reason::any(), Url::binary()}.
validate_jwt_get_rsp(JwtData, Referer) ->
    update_rsp_on_success(validate_jwt(JwtData), Referer).


%% @doc update the rsp with a new session containing the claims from the jwt
-spec update_rsp_on_success({ok, map(), rsp()} | {error, any(), rsp(), map()},
                            binary() ) -> {ok, rsp()} |
                                          {error, {bad_jwt, any()}, binary()}.
update_rsp_on_success({ok, #{claims := Claims}, Rsp}, Referer)
  when is_map(Claims) ->
    Success = jwt_get_success_url(Claims),
    Failed = jwt_get_failed_url(Claims),
    Subject = jwt_get_subject(Claims),
    ServiceId = jwt_get_service_id(Claims),
    Params = jwt_get_params(Claims),
    Provider = jwt_get_provider(Claims),
    Session = #watts_rsp_session{
                 sub = Subject,
                 service_id = ServiceId,
                 params = Params,
                 provider = Provider,
                 success_url = Success,
                 failed_url = Failed,
                 referer = Referer
                },
    {ok, Rsp#watts_rsp{session = Session}};
update_rsp_on_success({error, Error, Rsp, JwtMap}, Referer) ->
    {error, {bad_jwt, Error}, error_return_url(Rsp, JwtMap, Referer) }.


%% @doc validate the jwt with the keys of the rsp, if it exists.
%% A non existing rsp will result in an empty list of keys.
-spec validate_jwt(binary()) ->  {ok, map(), rsp()} |
                                 {error, Reason::any(), rsp(), map()}.
validate_jwt(Jwt) ->
    {ok, JwtMap} = erljwt:to_map(Jwt),
    Rsp = get_rsp_from_jwt_map(JwtMap),
    {ok, Keys} = watts_rsp_keys:get_keys(Rsp#watts_rsp.key_pid),
    validate_jwt_content(erljwt:validate(Jwt, [rs256], #{}, Keys),
                 Rsp, JwtMap).

%% @doc get the rsp configuration from the jwt map
-spec get_rsp_from_jwt_map(map() | any()) -> rsp().
get_rsp_from_jwt_map(#{claims := #{ iss := Iss}}) ->
    get_info(Iss);
get_rsp_from_jwt_map(_) ->
    #watts_rsp{}.



%% @doc validate the result of the parsed jwt, or the error.
%% Ensure the content of the jwt is valid and the service and the provider
%% do exist.
-spec validate_jwt_content({ok, map()} | {error, any()}, rsp(), map())
                          -> {ok, map(), rsp()} |
                             {error, Reason::any(), rsp(), map()} .
validate_jwt_content({ok, #{ claims := #{iss := _Iss,
                            sub := _Sub,
                            exp := _Exp,
                            iat := _Iat,
                            watts_service := Service
                           } = Claims,
                     header := #{ typ := <<"watts-rsp">> }
              } = Jwt }, Rsp, JwtMap) ->
    ServiceExists = watts_service:exists(Service),
    Provider = maps:get(watts_provider, Claims, undefined),
    ProviderExists = provider_exists(Provider),
    maybe_return_jwt(ServiceExists, ProviderExists, Jwt, Rsp, JwtMap);
validate_jwt_content({ok, _}, Rsp, JwtMap) ->
    {error, invalid_content, Rsp, JwtMap};
validate_jwt_content({error, Error}, Rsp, JwtMap) ->
    {error, Error, Rsp, JwtMap}.

%% @doc check if the provider exists, if not used return true
-spec provider_exists(any()) -> boolean().
provider_exists(undefined) ->
    true;
provider_exists(Id) when is_binary(Id) ->
    {OkError, _} = oidcc:find_openid_provider(Id),
    OkError == ok;
provider_exists(_) ->
    false.

%% @doc return the jwt if service and provider exist, else error
-spec maybe_return_jwt(boolean(), boolean(), map(), rsp(), map())
                      -> {ok, map(), rsp()} |
                         {error, Reason::any(), rsp(), map()} .
maybe_return_jwt(true, true, Jwt, Rsp, _) ->
    {ok, Jwt, Rsp};
maybe_return_jwt(false, _, _, Rsp, JwtMap) ->
    {error, service_missing, Rsp, JwtMap};
maybe_return_jwt(_, false, _, Rsp, JwtMap) ->
    {error, provider_missing, Rsp, JwtMap}.




%% @doc get the success url from the jwt
-spec jwt_get_success_url(Claims :: map()) -> binary() | undefined.
jwt_get_success_url(Claims) ->
    maps:get(success_url, Claims, undefined).

%% @doc get the failed url from the jwt
-spec jwt_get_failed_url(Claims :: map()) -> binary() | undefined.
jwt_get_failed_url(Claims) ->
    maps:get(failed_url, Claims, undefined).

%% @doc get the provider from the jwt
-spec jwt_get_provider(Claims :: map()) -> binary() | undefined.
jwt_get_provider(Claims) ->
    maps:get(watts_provider, Claims, undefined).

%% @doc get the subject from the jwt
-spec jwt_get_subject(Claims :: map()) -> binary() | undefined.
jwt_get_subject(Claims) ->
    maps:get(sub, Claims, undefined).

%% @doc get the service id from the jwt
-spec jwt_get_service_id(Claims :: map()) -> binary().
jwt_get_service_id(Claims) ->
    maps:get(watts_service, Claims).

%% @doc the the function params from the jwt
-spec jwt_get_params(map()) -> map().
jwt_get_params(Claims) ->
    Params = maps:get(watts_params, Claims, #{}),
    ensure_map(Params).

%% @doc convert a list into a map, if needed.
-spec ensure_map(map() | list()) -> map().
ensure_map(Map) when is_map(Map) ->
    Map;
ensure_map(MapList) when is_list(MapList) ->
    maps:from_list(MapList);
ensure_map(_) ->
    #{}.

%% @doc return back on error, either to the failed url, if valid, or referer
-spec error_return_url(rsp(), map(), binary()) -> binary().
error_return_url( #watts_rsp{base_url = Base}, #{claims := Claims}, Referer) ->
    FailedUrl = jwt_get_failed_url(Claims),
    FromRsp = is_valid_url(Referer, Base),
    ValidFail = is_valid_url(FailedUrl, Base),
    failed_or_referer(FromRsp and ValidFail, FailedUrl, Referer);
error_return_url(_, _, Referer) ->
    Referer.

%% @doc use the passed failure url, if valid, else referer url
-spec failed_or_referer(boolean(), binary(), binary()) -> binary().
failed_or_referer(true, FailedUrl, _) ->
    FailedUrl;
failed_or_referer(_, _, Referer) ->
    Referer.
