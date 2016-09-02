-module(tts_oidc_client).
-behaviour(oidcc_client).
-include("tts.hrl").

-export([
         login_succeeded/1,
         login_failed/2
        ]).

-define(COOKIE, <<"tts_session">>).

login_succeeded(#{id := #{claims := #{ sub := Subject, iss := Issuer}},
                 access := #{token := AccessToken}} = TokenMap) ->
    {ok, SessPid} = tts_session_mgr:new_session(),
    try
        {ok, UserInfo} = tts_user_cache:get_user_info(Issuer, Subject, 
                                                      AccessToken),
        {ok, SessId} = tts_session:get_id(SessPid),
        ok = tts_session:set_token(TokenMap, SessPid),
        ok = tts_session:set_user_info(UserInfo, SessPid),
        {ok, MaxAge} = tts_session:get_max_age(SessPid),
        lager:debug("login success"),
        Opts = create_cookie_opts(MaxAge), 
        {ok, [{redirect, ?CONFIG(ep_user)}, {cookie, ?COOKIE, SessId, Opts}]}
    catch _:_ ->
            %% TODO:
            %% show an error on the login page 
            tts_session:close(SessPid),
            lager:debug("login internal fail"),
            {ok, [{redirect, ?CONFIG(ep_main)}]}
    end;
login_succeeded(_BadToken) -> 
    %% TODO:
    %% show an error on the login page 
    lager:debug("bad token"),
    {ok, [{redirect, ?CONFIG(ep_main)}]}.

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
