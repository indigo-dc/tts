-module(tts_http_prep_test).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
          bad_request = false,
          config_loaded = undefined,
          req_map = #{}
         }).

init_not_loaded_test() ->
    application:unset_env(tts, conf_loaded),
    ?assertEqual(false, tts_config:is_loaded()),

    Req = doesnt_matter,
    {ok, Req, #state{config_loaded = ConfLoaded}} = tts_http_prep:init(ignored,
                                                                         Req,
                                                                         ignored),
    ?assertEqual(false, ConfLoaded),
    ok.

init_simple_bad_request_test() ->
    application:set_env(tts, conf_loaded, true),
    ?assertEqual(true, tts_config:is_loaded()),

    Req = bad_request,
    {ok, Req, #state{bad_request = BadRequest}} = tts_http_prep:init(ignored,
                                                                         Req,
                                                                         ignored),
    ?assertEqual(true, BadRequest),
    application:unset_env(tts, conf_loaded),
    ok.

init_good_request_test() ->
    application:set_env(tts, ep_user,<<"/user">>),
    application:set_env(tts, ep_return,<<"/oidc/return">>),
    application:set_env(tts, ep_redirect,<<"/oidc/redirect">>),

    MeckModules = [cowboy_req, tts_session_mgr, tts_session],

    ReqPath = fun(Req) ->  {<<"/user">>, Req} end,
    ReqQs = fun(Req) ->  {[], Req} end,
    ReqCookie = fun(_, Req) ->  {<<"123">>, Req} end,
    ReqBodyQs = fun(Req) ->  {ok, [
                                   {<<"action">>, <<"unknown">>},
                                   {<<"action">>, <<"request">>},
                                   {<<"code">>, <<"789">>},
                                   {<<"id">>, <<"14">>},
                                   {<<"unknown">>, <<"14">>}
                                  ], Req} end,
    ReqHeaders = fun(Req) ->  {[{<<"user-agent">>, <<"unit-test">>}], Req} end,
    ReqMethod = fun(Req) ->  {<<"GET">>, Req} end,
    ReqPeer = fun(Req) ->  {{{127,0,0,1},8080}, Req} end,

    MgrSession = fun(CookieId) ->
                         CookieId = <<"123">>,
                         {ok, self()}
                 end,
    SessId = fun(Pid) ->
                     Pid = self(),
                     {ok, 456}
             end,
    SessLoggedIn = fun(Pid) ->
                           Pid = self(),
                           true
                   end,

    SessIsUA = fun(Agent, Pid) ->
                       Agent = <<"unit-test">>,
                       Pid = self(),
                       true
               end,
    SessIsIP = fun(Ip, Pid) ->
                       Ip = {127,0,0,1},
                       Pid = self(),
                       true
               end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, path, ReqPath),
    ok = meck:expect(cowboy_req, qs_vals, ReqQs),
    ok = meck:expect(cowboy_req, cookie, ReqCookie),
    ok = meck:expect(cowboy_req, body_qs, ReqBodyQs),
    ok = meck:expect(cowboy_req, headers, ReqHeaders),
    ok = meck:expect(cowboy_req, method, ReqMethod),
    ok = meck:expect(cowboy_req, peer, ReqPeer),
    ok = meck:expect(tts_session_mgr, get_session, MgrSession),
    ok = meck:expect(tts_session, get_id, SessId),
    ok = meck:expect(tts_session, is_logged_in, SessLoggedIn),
    ok = meck:expect(tts_session, is_user_agent, SessIsUA),
    ok = meck:expect(tts_session, is_same_ip, SessIsIP),


    application:set_env(tts, conf_loaded, true),
    ?assertEqual(true, tts_config:is_loaded()),

    Req = bad_request,
    {ok, Req, #state{bad_request = BadRequest}} = tts_http_prep:init(ignored,
                                                                     Req,
                                                                     ignored),
    ?assertEqual(false, BadRequest),
    ok = test_util:meck_done(MeckModules),
    application:unset_env(tts, conf_loaded),
    ok.
