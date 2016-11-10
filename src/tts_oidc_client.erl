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
    %% show an error page with redirect to login
    lager:warning("login failed: ~p - ~p",[Reason, Description]),
    {ok, [{redirect, ?CONFIG(ep_main)}]}.
