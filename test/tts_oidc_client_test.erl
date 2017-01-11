-module(tts_oidc_client_test).
-include_lib("eunit/include/eunit.hrl").

succeed_test() ->
    {ok, Meck} = start_meck(),
    {ok, _} = watts_oidc_client:login_succeeded(#{token => bad}),
    {ok, _} = watts_oidc_client:login_succeeded(#{token => good}),
    ok = stop_meck(Meck),
    ok.




fail_test() ->
    {ok, Meck} = start_meck(),
    {ok, _} = watts_oidc_client:login_failed(internal, {token_invalid, blah}),
    {ok, _} = watts_oidc_client:login_failed(internal, {bad_user_agent, blah}),
    {ok, _} = watts_oidc_client:login_failed(internal, {bad_peer_ip, blah}),
    {ok, _} = watts_oidc_client:login_failed(internal, {bad_cookie, blah}),
    {ok, _} = watts_oidc_client:login_failed(session_not_found,  blah),
    {ok, _} = watts_oidc_client:login_failed(blah,  blah),
    ok = stop_meck(Meck),
    ok.


start_meck() ->
    MeckModules = [tts, tts_session_mgr],
    {ok, Pid1} = tts_session:start_link(<<"1">>),
    ok = tts_session:set_token(#{}, Pid1),
    {ok, Pid2} = tts_session:start_link(<<"2">>),
    ok = tts_session:set_token(#{}, Pid2),
    Login = fun(TokenMap) ->
                    case TokenMap of
                        #{token := good} ->
                            {ok, #{session_pid => Pid1}};
                        _ ->
                            {error, bad_token}
                    end
            end,
    ErrorSession = fun(_Msg) ->
                           {ok, Pid2}
                   end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts, login_with_oidcc, Login),
    ok = meck:expect(tts, session_with_error, ErrorSession),
    ok = meck:expect(tts_session_mgr, session_terminating, fun(_) -> ok end),
    {ok, {MeckModules, Pid1, Pid2}}.


stop_meck({MeckModules, Pid1, Pid2}) ->
    ok = tts_session:close(Pid1),
    ok = tts_session:close(Pid2),
    ok = test_util:meck_done(MeckModules),
    ok.
