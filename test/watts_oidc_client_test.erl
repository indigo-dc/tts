-module(watts_oidc_client_test).
-include_lib("eunit/include/eunit.hrl").

succeed_test() ->
    {ok, Meck} = start_meck(),
    try
        EnvMap = #{req => req},
        {ok, _} = watts_oidc_client:login_succeeded(#{token => bad}, EnvMap),
        {ok, _} = watts_oidc_client:login_succeeded(#{token => oidc_good}, EnvMap),
        {ok, _} = watts_oidc_client:login_succeeded(#{token => rsp_ui_good}, EnvMap),
        {ok, _} = watts_oidc_client:login_succeeded(#{token => rsp_good}, EnvMap),
        ok
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
    MeckModules = [watts, watts_rsp, watts_session_mgr, cowboy_req],
    {ok, Pid1} = watts_session:start_link(<<"1">>),
    ok = watts_session:set_token(#{}, Pid1),
    ok = watts_session:set_type(oidc, Pid1),
    {ok, Pid2} = watts_session:start_link(<<"2">>),
    ok = watts_session:set_token(#{}, Pid2),
    ok = watts_session:set_type({rsp, login, ui}, Pid2),
    ok = watts_session:set_redirection(<<"test1">>, [], <<"iam">>, Pid2),
    {ok, Pid3} = watts_session:start_link(<<"3">>),
    ok = watts_session:set_token(#{}, Pid3),
    ok = watts_session:set_redirection(<<"test2">>, [], <<"iam">>, Pid3),
    ok = watts_session:set_rsp(rsp3, Pid3),
    ok = watts_session:set_type({rsp, login, no_ui}, Pid3),
    Login = fun(TokenMap, _) ->
                    case TokenMap of
                        #{token := oidc_good} ->
                            {ok, #{session_pid => Pid1, session_type => oidc}};
                        #{token := rsp_ui_good} ->
                            {ok, #{session_pid => Pid2, session_type => {rsp, ui, login}}};
                        #{token := rsp_good} ->
                            {ok, #{session_pid => Pid3, session_type => {rsp, no_ui, login}}};
                        _ ->
                            {error, bad_token}
                    end
            end,
    ErrorSession = fun(_Msg) ->
                           {ok, Pid2}
                   end,
    ReqCred = fun(<<"test2">>, _, _) ->
                      {ok, something}
              end,

    RetUrls = fun(rsp3) ->
                      {url_good3, url_bad3}
              end,

    ReturnSession = fun(_) ->
                            {ok, Pid1}
                    end,
    Cookies = fun(Req) -> {[{watts_http_util:cookie_name(), <<"data">>}], Req} end,
    Peer = fun(Req) -> {{{127, 0, 0 , 1},234}, Req} end,
    Header = fun(_Name, Req) -> {<<>>, Req} end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts, login_with_oidcc, Login),
    ok = meck:expect(watts, session_with_error, ErrorSession),
    ok = meck:expect(watts, request_credential_for, ReqCred),
    ok = meck:expect(watts, logout, fun(_) -> ok end),
    ok = meck:expect(watts_rsp, get_return_urls, RetUrls),
    ok = meck:expect(watts_session_mgr, session_terminating, fun(_) -> ok end),
    ok = meck:expect(watts_session_mgr, get_session, ReturnSession),
    ok = meck:expect(cowboy_req, cookies, Cookies),
    ok = meck:expect(cowboy_req, peer, Peer),
    ok = meck:expect(cowboy_req, header, Header),
    {ok, {MeckModules, Pid1, Pid2, Pid3}}.


stop_meck({MeckModules, Pid1, Pid2, Pid3}) ->
    ok = watts_session:close(Pid1),
    ok = watts_session:close(Pid2),
    ok = watts_session:close(Pid3),
    ok = test_util:meck_done(MeckModules),
    ok.
