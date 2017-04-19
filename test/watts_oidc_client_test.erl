-module(watts_oidc_client_test).
-include_lib("eunit/include/eunit.hrl").

succeed_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, _} = watts_oidc_client:login_succeeded(#{token => bad}),
        {ok, _} = watts_oidc_client:login_succeeded(#{token => good})
    after
        ok = stop_meck(Meck)
    end,
    ok.




fail_test() ->
    {ok, Meck} = start_meck(),
    try
        {ok, _} = watts_oidc_client:login_failed(internal, {token_invalid, blah}),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_user_agent, blah}),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_peer_ip, blah}),
        {ok, _} = watts_oidc_client:login_failed(internal, {bad_cookie, blah}),
        {ok, _} = watts_oidc_client:login_failed(session_not_found,  blah),
        {ok, _} = watts_oidc_client:login_failed(blah,  blah)
    after
        ok = stop_meck(Meck)
    end,
    ok.


start_meck() ->
    MeckModules = [watts, watts_session_mgr],
    {ok, Pid1} = watts_session:start_link(<<"1">>),
    ok = watts_session:set_token(#{}, Pid1),
    {ok, Pid2} = watts_session:start_link(<<"2">>),
    ok = watts_session:set_token(#{}, Pid2),
    Login = fun(TokenMap) ->
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
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(watts, login_with_oidcc, Login),
    ok = meck:expect(watts, session_with_error, ErrorSession),
    ok = meck:expect(watts_session_mgr, session_terminating, fun(_) -> ok end),
    {ok, {MeckModules, Pid1, Pid2}}.


stop_meck({MeckModules, Pid1, Pid2}) ->
    ok = watts_session:close(Pid1),
    ok = watts_session:close(Pid2),
    ok = test_util:meck_done(MeckModules),
    ok.
