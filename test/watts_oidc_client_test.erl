-module(watts_oidc_client_test).
-include_lib("eunit/include/eunit.hrl").

succeed_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, _} = watts_oidc_client:login_succeeded(#{token => bad}, type),
        {ok, _} = watts_oidc_client:login_succeeded(#{token => good}, type)
    after
        ok = stop_meck(Meck)
    end,
    ok.




fail_test() ->
    {ok, Meck} = start_meck(),
    try
        EnvMap = #{req => req},
        {ok, _} = watts_oidc_client:login_failed(internal, {token_invalid, blah}, EnvMap),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_user_agent, blah}, EnvMap),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_peer_ip, blah}, EnvMap),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_cookie, blah}, EnvMap),
        {ok, _} = watts_oidc_client:login_failed(session_not_found,  blah, EnvMap),
        {ok, _} = watts_oidc_client:login_failed(blah,  blah, EnvMap)
    after
        ok = stop_meck(Meck)
    end,
    ok.


start_meck() ->
    MeckModules = [watts, watts_session_mgr, cowboy_req],
    {ok, Pid1} = watts_session:start_link(<<"1">>),
    ok = watts_session:set_token(#{}, Pid1),
    ok = watts_session:set_type(oidc, Pid1),
    {ok, Pid2} = watts_session:start_link(<<"2">>),
    ok = watts_session:set_token(#{}, Pid2),
    ok = watts_session:set_type({rsp, login, ui}, Pid2),
    Login = fun(TokenMap, _) ->
                    case TokenMap of
                        #{token := good} ->
                            {ok, #{session_pid => Pid1, session_type => oidc}};
                        _ ->
                            {error, bad_token}
                    end
            end,
    ErrorSession = fun(_Msg) ->
                           {ok, Pid2}
                   end,
    ReturnSession = fun(_) ->
                            {ok, Pid1}
                    end,
    Cookies = fun(Req) -> {[{watts_http_util:cookie_name(), <<"data">>}], Req} end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts, login_with_oidcc, Login),
    ok = meck:expect(watts, session_with_error, ErrorSession),
    ok = meck:expect(watts_session_mgr, session_terminating, fun(_) -> ok end),
    ok = meck:expect(watts_session_mgr, get_session, ReturnSession),
    ok = meck:expect(cowboy_req, cookies, Cookies),
    {ok, {MeckModules, Pid1, Pid2}}.


stop_meck({MeckModules, Pid1, Pid2}) ->
    ok = watts_session:close(Pid1),
    ok = watts_session:close(Pid2),
    ok = test_util:meck_done(MeckModules),
    ok.
