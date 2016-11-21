-module(tts_oidc_client).
-behaviour(oidcc_client).
-include("tts.hrl").

-export([
         login_succeeded/1,
         login_failed/2
        ]).

login_succeeded(TokenMap) ->
    case tts:login_with_oidcc(TokenMap) of
        {ok, #{session_pid := SessPid}} ->
            redirect_set_cookie(SessPid);
        {error, Reason} ->
            lager:warning("login failed internal: ~p", [Reason]),
            ErrMsg = bin_error_msg(login, undefind),
            redirect_error(ErrMsg)
    end.

login_failed(Reason, Details) ->
    lager:warning("login failed: ~p - ~p", [Reason, Details]),
    ErrMsg = bin_error_msg(Reason, Details),
    redirect_error(ErrMsg).

redirect_error(ErrorMsg) ->
    {ok, SessPid} = tts:session_with_error(ErrorMsg),
    redirect_set_cookie(SessPid).

redirect_set_cookie(SessPid) ->
    {ok, SessToken} = tts_session:get_sess_token(SessPid),
    {ok, MaxAge} = tts_session:get_max_age(SessPid),
    Opts = tts_http_util:create_cookie_opts(MaxAge),
    CookieName = tts_http_util:cookie_name(),
    {ok, [{redirect, ?CONFIG(ep_main)},
          {cookie, CookieName, SessToken, Opts}]}.

bin_error_msg(Reason, Details) ->
    list_to_binary(error_msg(Reason, Details)).

error_msg(login, _) ->
    "sorry, an internal error occured, please try again";
error_msg(internal, {token_invalid, _}) ->
    "the returned token was invalid, the error has been logged";
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
