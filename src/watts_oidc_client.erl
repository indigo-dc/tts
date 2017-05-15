-module(watts_oidc_client).
-behaviour(oidcc_client).
-include("watts.hrl").

-export([
         login_succeeded/1,
         login_failed/2
        ]).

login_succeeded(TokenMap) ->
    case watts:login_with_oidcc(TokenMap) of
        {ok, #{session_pid := SessPid, session_type := {rsp, _, login} }} ->
            {ok, #{service := Service,
                   params := Params
                  }} = watts_session:get_redirection(SessPid),
            Result = watts:request_credential_for(Service, SessPid, Params),
            redirect_back(Result, SessPid);
        {ok, #{session_pid := SessPid, session_type := oidc }} ->
            redirect_set_cookie(SessPid);
        {error, Reason} ->
            lager:warning("login failed internal: ~p", [Reason]),
            ErrMsg = bin_error_msg(login, undefind),
            redirect_error(ErrMsg)
    end.

login_failed(Reason, Details) ->
    %% todo redirect back on to rsp if RSP session
    lager:warning("login failed: ~p - ~p", [Reason, Details]),
    ErrMsg = bin_error_msg(Reason, Details),
    redirect_error(ErrMsg).

redirect_back(_Result, SessPid) ->
    {ok, Rsp} = watts_session:get_rsp(SessPid),
    ok = watts:logout(SessPid),
    {ok, [{redirect, watts_rsp:get_return_url(Rsp)},
          set_cookie(undefined)]}.


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
