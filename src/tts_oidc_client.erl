-module(tts_oidc_client).
-behaviour(oidcc_client).
-include("tts.hrl").

-export([
         login_succeeded/1,
         login_failed/2
        ]).


-define(COOKIE, <<"tts_session">>).

login_succeeded(TokenMap) ->
    case tts:login_with_oidcc(TokenMap) of
        {ok, #{session_id := SessId, session_pid := SessPid}} ->
            {ok, MaxAge} = tts_session:get_max_age(SessPid),
            Opts = create_cookie_opts(MaxAge),
            {ok, [{redirect, ?CONFIG(ep_main)},
                  {cookie, ?COOKIE, SessId, Opts}]};
        {error, _} ->
            %% TODO:
            %% show an error on the login page
            {ok, [{redirect, ?CONFIG(ep_main)}]}
    end.

login_failed(_Reason, _Description) ->
    %% TODO:
    %% show an error on the login page
    lager:debug("login failed"),
    {ok, [{redirect, ?CONFIG(ep_main)}]}.


create_cookie_opts(MaxAge) ->
    BasicOpts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    case ?CONFIG(ssl) of
        true ->
            [{secure, true} | BasicOpts];
        _ ->
            BasicOpts
    end.
