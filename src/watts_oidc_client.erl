-module(watts_oidc_client).
-behaviour(oidcc_client).
-include("watts.hrl").

-export([
         login_succeeded/2,
         login_failed/3
        ]).

login_succeeded(TokenMap, EnvMap) ->
    case watts:login_with_oidcc(TokenMap, get_session_type(EnvMap)) of
        {ok, #{session_pid := SessPid, session_type := {rsp, no_ui, login} }} ->
            {ok, #{service := Service,
                   params := Params
                  }} = watts_session:get_redirection(SessPid),
            Result = watts:request_credential_for(Service, SessPid, Params),
            redirect_back(Result, SessPid);
        {ok, #{session_pid := SessPid, session_type := {rsp, ui, login} }} ->
            redirect_set_cookie(SessPid);
        {ok, #{session_pid := SessPid, session_type := oidc }} ->
            redirect_set_cookie(SessPid);
        {error, Reason} ->
            lager:warning("login failed internal: ~p", [Reason]),
            ErrMsg = bin_error_msg(login, undefind),
            redirect_error(ErrMsg)
    end.

login_failed(Reason, Details, EnvMap) ->
    lager:warning("login failed: ~p - ~p", [Reason, Details]),
    {SessType, Sess} = get_session_type(EnvMap),
    handle_failed_for_session(SessType, Sess, Reason, Details).

handle_failed_for_session({rsp, _, _} , SessPid, Reason, _Details) ->
    redirect_back({error, Reason}, SessPid);
handle_failed_for_session(_, _, Reason, Details) ->
    ErrMsg = bin_error_msg(Reason, Details),
    redirect_error(ErrMsg).

redirect_back(Result, SessPid) ->
    {ok, Rsp} = watts_session:get_rsp(SessPid),
    ok = watts:logout(SessPid),
    {ok, [{redirect, get_return_url(Result, Rsp)}, set_cookie(undefined)]}.

get_return_url({ok, _}, Rsp) ->
    {SuccessUrl, _} = watts_rsp:get_return_urls(Rsp),
    SuccessUrl;
get_return_url(_, Rsp) ->
    {_, ErrorUrl} = watts_rsp:get_return_urls(Rsp),
    ErrorUrl.



redirect_error(ErrorMsg) ->
    {ok, SessPid} = watts:session_with_error(ErrorMsg),
    redirect_set_cookie(SessPid).

redirect_set_cookie(SessPid) ->
    {ok, [{redirect, ?CONFIG(ep_main)},
          set_cookie(SessPid)]}.

set_cookie(SessPid) when is_pid(SessPid) ->
    {ok, SessToken} = watts_session:get_sess_token(SessPid),
    {ok, MaxAge} = watts_session:get_max_age(SessPid),
    Opts = watts_http_util:create_cookie_opts(MaxAge),
    CookieName = watts_http_util:cookie_name(),
    {cookie, CookieName, SessToken, Opts};
set_cookie(_)  ->
    CookieName = watts_http_util:cookie_name(),
    Opts = watts_http_util:create_cookie_opts(0),
    {cookie, CookieName, <<"deleted">>, Opts}.



bin_error_msg(Reason, Details) ->
    list_to_binary(error_msg(Reason, Details)).

error_msg(login, _) ->
    "sorry, an internal error occured, please try again";
error_msg(internal,
          {token_invalid, {error, {required_fields_missing, [Field] } } }) ->
    M1 = "The id-token returned from your IdP is missing the ",
    M2 = io_lib:format("required field '~p'. Please contact your IdP.",
                       [Field]),
    io_lib:format("~s~s", [M1, M2]);
error_msg(oidc_provider_error, Error) ->
    M1 = io_lib:format("Your IdP returned you with the error ~p.", [Error]),
    M2 = "Please contact your IdP.",
    io_lib:format("~s ~s", [M1, M2]);
error_msg(internal, {token_invalid, {error, no_id_token}}) ->
    "The token from your IdP is missing an id-token. Please contact your IdP";
error_msg(internal, {token_invalid, _}) ->
    "The token returned from your IdP was invalid, the error has been logged";
error_msg(internal, {bad_user_agent, _}) ->
    "you are not who you was before, incident has been logged";
error_msg(internal, {bad_peer_ip, _}) ->
    "stealing cookies is a crime, you have been logged";
error_msg(internal, {bad_cookie, _}) ->
    "sorry, been unable to recognize you, please try again.";
error_msg(session_not_found, _) ->
    "sorry, your session expired during the login process, please try again.";
error_msg(_, _) ->
    "sorry, something went wrong, please try again".


get_session_type(#{req := Req}) ->
    {Cookies, _} = cowboy_req:cookies(Req),
    Cookie = lists:keyfind(watts_http_util:cookie_name(), 1, Cookies),
    get_session_type(Cookie);
get_session_type({_, Cookie}) when is_binary(Cookie) ->
    Result =  watts_session_mgr:get_session(Cookie),
    get_session_type(Result);
get_session_type({ok, Pid}) when is_pid(Pid) ->
    {ok, Type} = watts_session:get_type(Pid),
    {Type, Pid};
get_session_type(_) ->
    {none, undefined}.
