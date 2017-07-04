-module(watts_http_rsp_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

init_test() ->
    {ok, b, []} = watts_http_rsp:init(a, b, c).

terminate_test() ->
   ok = watts_http_rsp:terminate(a, b, c).

no_ui_no_login_handle_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req1, state)
    after
        stop_meck(Meck)
    end,
    ok.

no_ui_login_handle_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req2, state)
    after
        stop_meck(Meck)
    end,
    ok.

ui_no_login_handle_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req3, state)
    after
        stop_meck(Meck)
    end,
    ok.

ui_login_handle_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req4, state)
    after
        stop_meck(Meck)
    end,
    ok.

setup_session_fail1_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req5, state)
    after
        stop_meck(Meck)
    end,
    ok.

setup_session_fail2_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req6, state)
    after
        stop_meck(Meck)
    end,
    ok.

setup_request_type_fail_test() ->
    {ok, Meck} = start_meck(),
    try
        watts_http_rsp:handle(req7, state)
    after
        stop_meck(Meck)
    end,
    ok.


start_meck() ->

    Path = fun(req1) ->
                   {<<"rsp/jwt1">>, req1};
              (req2) ->
                   {<<"rsp/jwt2">>, req2};
              (req3) ->
                   {<<"rsp/jwt3">>, req3};
              (req4) ->
                   {<<"rsp/jwt4">>, req4};
              (req5) ->
                   {<<"rsp/jwt5">>, req5};
              (req6) ->
                   {<<"rsp/jwt6">>, req6};
              (req7) ->
                   {<<"rsp/jwt7">>, req7}
           end,

    Header = fun(<<"referer">>, Req) ->
                     {<<"https://rsp.watts.kit.edu">>, Req}
             end,

    Reply = fun(400, req6) ->
                    {ok, req6}
            end,

    Validate = fun(<<"jwt1">>, _Ref) ->
                       {ok, rsp1};
                  (<<"jwt2">>, _) ->
                       {ok, rsp2};
                  (<<"jwt3">>, _) ->
                       {ok, rsp3};
                  (<<"jwt4">>, _) ->
                       {ok, rsp4};
                  (<<"jwt5">>, _) ->
                       {error, bad_jwt, url_bad5};
                  (<<"jwt6">>, _) ->
                       totally_bad_jwt;
                  (<<"jwt7">>, _) ->
                       {ok, rsp7}
               end,

    ReqType = fun(rsp1) ->
                      rsp_no_ui_no_login;
                 (rsp2) ->
                      rsp_no_ui_login;
                 (rsp3) ->
                      rsp_ui_no_login;
                 (rsp4) ->
                      rsp_ui_login;
                 (rsp7) ->
                      {error, no_working}
              end,

    ServData = fun(_) ->
                       {serv, param}
               end,
    RetUrls = fun(rsp1) ->
                      {url_good1, url_bad1};
                 (rsp2) ->
                      {url_good2, url_bad2};
                 (rsp3) ->
                      {url_good3, url_bad3};
                 (rsp4) ->
                      {url_good4, url_bad4};
                 (rsp7) ->
                      {url_good7, url_bad7}
               end,

    Provider = fun(rsp2) ->
                       <<"iam">>;
                  (rsp4) ->
                       <<"google">>
               end,

    Session = fun(rsp1) ->
                      {ok, sess1};
                 (rsp2) ->
                      {ok, sess2};
                 (rsp3) ->
                      {ok, sess3};
                 (rsp4) ->
                      {ok, sess4};
                 (rsp7) ->
                      {ok, sess7}
              end,

    ReqCred = fun(_, sess1, _) ->
                      {ok, cred1};
                 (_, sess2, _) ->
                      {ok, cred2};
                 (_, sess3, _) ->
                      {ok, cred3};
                 (_, sess4, _) ->
                      {ok, cred4}
              end,

    Logout = fun(sess7) ->
                     ok
             end,

    Token = fun(sess1) ->
                    {ok, token1};
               (sess2) ->
                    {ok, token2};
               (sess3) ->
                    {ok, token3};
               (sess4) ->
                    {ok, token4};
               (sess7) ->
                    {ok, token7}
            end,

    Redir = fun(url_good1, req1) ->
                    ok;
               (url_prov2, req2) ->
                    ok;
               (url_ui3, req3) ->
                    ok;
               (url_prov4, req4) ->
                    ok;
               (url_bad5, req5) ->
                    ok;
               (url_bad7, req7) ->
                    ok;
               (Url, Req) ->
                    io:format("bad url ~p (~p)~n", [Url, Req]),
                    throw(bad_redir)
            end,

    CookieAction = fun(update, _, _, req1) ->
                           {ok, req1};
                      (update, _, _, req2) ->
                           {ok, req2};
                      (update, _, _, req3) ->
                           {ok, req3};
                      (update, _, _, req4) ->
                           {ok, req4};
                      (update, _, _, req7) ->
                           {ok, req7};
                      (Action, _, _, Req) ->
                           io:format("invalid cookie action: ~p ~p", [Action, Req]),
                           throw(bad_cookie_action)
                   end,

    RelPath = fun("oidc?provider=iam") ->
                      url_prov2;
                 ("") ->
                      url_ui3;
                 ("oidc?provider=google") ->
                      url_prov4;
                 (Other) ->
                      io:format("unsupported rel path ~p~n", [Other]),
                      throw(bad_rel_path)
              end,

    MeckModules = [cowboy_req, watts_rsp, watts, watts_session, watts_http_util],
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, path, Path),
    ok = meck:expect(cowboy_req, header, Header),
    ok = meck:expect(cowboy_req, reply, Reply),
    ok = meck:expect(watts_rsp, validate_jwt_get_rsp, Validate),
    ok = meck:expect(watts_rsp, request_type, ReqType),
    ok = meck:expect(watts_rsp, get_service_data, ServData),
    ok = meck:expect(watts_rsp, get_return_urls, RetUrls),
    ok = meck:expect(watts_rsp, get_provider, Provider),
    ok = meck:expect(watts, session_for_rsp, Session),
    ok = meck:expect(watts, request_credential_for, ReqCred),
    ok = meck:expect(watts, logout, Logout),
    ok = meck:expect(watts_session, get_max_age, fun(_) -> {ok, 900} end ),
    ok = meck:expect(watts_session, get_sess_token, Token ),
    ok = meck:expect(watts_http_util, redirect_to, Redir ),
    ok = meck:expect(watts_http_util, perform_cookie_action, CookieAction ),
    ok = meck:expect(watts_http_util, relative_path, RelPath ),
    {ok, MeckModules}.


stop_meck(MeckModules) ->
    ok = test_util:meck_done(MeckModules),
    ok.
