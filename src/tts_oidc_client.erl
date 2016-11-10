-module(tts_oidc_client).
-behaviour(oidcc_client).
-include("tts.hrl").

-export([
         login_succeeded/1,
         login_failed/2
        ]).

login_succeeded(TokenMap) ->
    case tts:login_with_oidcc(TokenMap) of
        {ok, #{session_token := Token, session_pid := SessPid}} ->
            {ok, MaxAge} = tts_session:get_max_age(SessPid),
            Opts = tts_http_util:create_cookie_opts(MaxAge),
            CookieName = tts_http_util:cookie_name(),
            {ok, [{redirect, ?CONFIG(ep_main)},
                  {cookie, CookieName, Token, Opts}]};
        {error, _} ->
            %% TODO:
            %% show an error on the login page
            {ok, [{redirect, ?CONFIG(ep_main)}]}
    end.

login_failed(Reason, Description) ->
    %% TODO:
    %% start a session to keep track of the error
    lager:warning("login failed: ~p - ~p", [Reason, Description]),
    {ok, [{redirect, ?CONFIG(ep_main)}]}.


error_msg(internal, {token_invalid, _}) ->
    io_lib:format("the returned token was invalid, the error has been logged");
error_msg(internal, {bad_user_agent, _}) ->
    io_lib:format("you are not who you was before, incident has been logged");
error_msg(internal, {bad_peer_ip, _}) ->
    io_lib:format("stealing cookies is a crime, you have been logged");
error_msg(internal, {bad_cookie, _}) ->
    io_lib:format("sorry, something went wrong, please try again");
error_msg(_, _) ->
    io_lib:format("sorry, something went wrong, please try again").
