-module(watts_http_api_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").


-define(ISSUER_URL, <<"https://test.tts.somewhere">>).


init_test() ->
    ?assertEqual({upgrade, protocol, cowboy_rest}, watts_http_api:init(a,b,c)),
    ok.

%% state from tts_rest
-record(state, {
          in = #{},
          method = undefined,
          version = undefined,
          type = undefined,
          id = undefined,
          queue_token = undefined,

          token = undefined,
          issuer = undefined,
          json = undefined,
          session_pid = undefined
         }).

rest_init_test() ->
    {ok, Meck} = start_meck(),
    try
        Req = #{path_info => [<<"v2">>,<<"info">>],
                        header => [],
                        method => <<"GET">>,
                        body => <<>>},
        {ok, Req, State} = watts_http_api:rest_init(Req, doesnt_matter),
        #{version := _Version,
          type := _Type,
          id := _Id,
          token := _Token,
          issuer := _Issuer,
          session := _CookieSession,
          method := _Method,
          content := _ContentType,
          body := _Body,
          header_used := _HeaderUsed} = State#state.in
    after
        ok = stop_meck(Meck)
    end,
    ok.

allowed_methods_test() ->
    State = #state{},
    Req = req,
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State} =
    watts_http_api:allowed_methods(Req, State),
    ok.

allow_missing_post_test() ->
    State = #state{},
    Req = req,
    {false, Req, State} = watts_http_api:allow_missing_post(Req, State).

content_types_provided_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = watts_http_api:content_types_provided(Req, State),
    {{<<"application">>, <<"json">>, '*'}, get_json} = ContentType,
    ok.

content_types_accepted_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = watts_http_api:content_types_accepted(Req, State),
    {{<<"application">>, <<"json">>, '*'}, post_json} = ContentType,
    ok.

malformed_request_test() ->
    {ok, Meck} = start_meck(),
    try
        Requests = [
                    % GOOD request
                    {#{ path_info => [<<"v1">>,<<"oidcp">>],
                        header => [],
                        method => <<"GET">>,
                        body => <<>>
                      }, false },

                    { #{ path_info => [<<"V1">>, <<"ID1">>, <<"service">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, false},

                    { #{ path_info => [<<"v2">>, <<"ID1">>, <<"credential">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, false},

                    { #{ path_info => [<<"v2">>, <<"ID1">>,
                                       <<"credential_data">>, <<"234">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, false },

                    { #{ path_info => [<<"v2">>, <<"ID1">>, <<"credential">>],
                         header => [{<<"content-type">>,{ok, {<<"application">>,
                                                              <<"json">>,[]}}}],
                         method => <<"POST">>,
                         body => <<"{\"service_id\":\"234\"}">>
                       }, false },
                    { #{ path_info => [<<"v2">>, <<"ID1">>, <<"credential">>, <<"234">>],
                         header => [
                                    {<<"authorization">>,<<"Bearer SomeToken">>}
                                   ],
                         method => <<"DELETE">>,
                         body => <<>>
                       }, false },
                    { #{ path_info => [<<"v2">>, <<"ID1">>, <<"credential">>, <<"234">>],
                         header => [
                                    {<<"authorization">>,<<"Bearer SomeToken">>}
                                   ],
                         method => <<"DELETE">>,
                         body => <<>>
                       }, false },

                    % BAD requests
                    { #{ path_info => [<<"latest">>, <<"ID1">>, <<"credential">>],
                         header => [],
                         method => <<"POST">>,
                         body => <<"{\"service_id\":234}">>
                       }, true },
                    { #{ path_info => [<<"latest">>, <<"ID1">>, <<"oidcp">>],
                         header => [],
                         method => <<"POST">>,
                         body => <<"no json">>
                       }, true },

                    { #{ path_info => [<<"latest">>, <<"ID1">>, <<"unknown_type">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, true },

                    { #{ path_info => [<<"v0">>, <<"ID1">>, <<"oidcp">>],
                         header => [{<<"authorization">>, <<"missingBearer">>}],
                         method => <<"GET">>,
                         body => <<>>
                       }, true },

                    { #{ path_info => [<<"v0">>, <<"ID2">>, <<"credentials">>],
                         header => [
                                    {<<"authorization">>,<<"Bearer SomeToken">>}
                                   ],
                         method => <<"GET">>,
                         body => <<>>
                       }, true },

                    { #{ path_info => [<<"vn">>, <<"oidcp">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, true },

                    { #{ path_info => [<<"234">>, <<"oidcp">>],
                         header => [],
                         method => <<"GET">>,
                         body => <<>>
                       }, true }
                   ],

        Test  = fun({Request, ExpResult}, _) ->
                        io:format("testing with request ~p~n",[Request]),
                        {ok, _Req, State} = watts_http_api:rest_init(Request,
                                                                    ignored),
                        io:format("got state ~p~n",[State]),
                        {Result, Request, _} =
                            watts_http_api:malformed_request(Request, State),
                        io:format("got result ~p, expecting ~p~n",[Result,
                                                                   ExpResult]),
                        ?assertEqual(ExpResult, Result),
                        ok
                end,
        ok = lists:foldl(Test,ok,Requests)
    after
        ok = stop_meck(Meck)
    end,
    ok.

is_authorized_test() ->
    {ok, Meck} = start_meck(),
    try
        Authz = <<"authorization">>,
        Mapping = [

                   {#state{}, {false, <<"authorization">>}},
                   {#state{type=some_unknown, token=defined}, {false, Authz}},
                   {#state{type=service}, {false, <<"authorization">>}},
                   {#state{type=service, issuer= <<"issuer">>}, {false, Authz}},
                   {#state{type=service, issuer= <<"issuer">>, token= <<"token">>}, {false, Authz}},
                   {#state{type=some_service, issuer= <<"issuer">>, token= <<"token">>}, {false, Authz}},

                   {#state{type=oidcp}, true},
                   {#state{type=info}, true},
                   {#state{type=logout}, true},
                   {#state{type=service, issuer= <<"issuer">>, token= <<"token">>}, {false, Authz}},
                   {#state{type=service, issuer= <<"unknown">>, token= <<"token">>}, {false, Authz}},
                   {#state{type=service, issuer= <<"known">>, token= <<"token">>}, {false, Authz}},
                   {#state{type=service, issuer= <<"known">>, token= <<"good1">>}, true},
                   {#state{type=service, session_pid=self()}, {false, Authz}},
                   {#state{type=unknown, session_pid=self()}, {false, Authz}},
                   {#state{type=service, issuer= <<"known">>, token= <<"good2">>}, true}
                  ],
        Req = req,
        Test = fun({State, ExpResult}, _AccIn) ->
                       io:format("testing with state ~p~n",[State]),
                       {Result, Req, _CState} = watts_http_api:is_authorized(Req, State),
                       io:format("got result ~p, expecting ~p~n",[Result, ExpResult]),
                       ?assertEqual(ExpResult, Result),
                       ok
               end,
        ok = lists:foldl(Test, ok, Mapping)
    after
        ok = stop_meck(Meck)
    end,
    ok.


resource_exists_test() ->
    {ok, Meck} = start_meck(),
    try
        Requests = [
                    %% good requests
                    {#state{id = undefined}, false},
                    {#state{id = undefined, type=oidcp}, true},
                    {#state{id = undefined, type=service}, true},
                    {#state{id = undefined, type=credential}, true},
                    {#state{id = <<"123">>, type=credential}, true},
                    {#state{type = cred_data, id= <<"someid">>}, true},

                    %% bad requests
                    {#state{id = <<"123">>, type=oidcp}, false},
                    {#state{id = <<"123">>, type=service}, false},
                    {#state{id = <<"124">>, type=oidcp}, false},
                    {#state{id = <<"124">>, type=service}, false},
                    {#state{type = creddata, id= <<"someid">>}, false},
                    {#state{type = creddata }, false}

                   ],

        Test  = fun({State, ExpResult}, _) ->
                        io:format("testing ~p, expecting: ~p~n", [State, ExpResult]),
                        {Result, req, _} = watts_http_api:resource_exists(req, State),
                        ?assertEqual(ExpResult, Result),
                        ok
                end,
        ok = lists:foldl(Test,ok,Requests)
    after
        ok = stop_meck(Meck)
    end,
    ok.

get_json_test() ->
    ?SETCONFIG(ep_main, <<"/">>),
    ?SETCONFIG(vsn, "latest"),
    ?SETCONFIG(enable_user_doc, false),
    ?SETCONFIG(enable_code_doc, false),

    {ok, Meck} = start_meck(),
    try
        Requests = [
                    {#state{version = latest,
                            type = service,
                            id = undefined,
                            session_pid = pid1,
                            method = get
                           },
                     #{<<"service_list">> => [#{<<"cred_limit">> => 1}]}
                    },
                    {#state{version = latest,
                            type = service,
                            id = undefined,
                            session_pid = pid2,
                            method = get
                           },
                     #{<<"service_list">> => []}
                    },
                    {#state{version = latest,
                            type = oidcp,
                            id = undefined,
                            session_pid = undefined,
                            method = get
                           },
                     #{<<"openid_provider_list">> =>
                           [#{<<"id">> => <<"ID1">>,
                              <<"issuer">> => <<"https://test.tts">>,
                              <<"ready">> => true},
                            #{<<"id">> => <<"ID2">>,
                              <<"issuer">> => <<"https://other.tts">>,
                              <<"ready">> => false}]}
                    },
                    {#state{version = latest,
                            type = cred_data,
                            id = <<"CRED1">>,
                            session_pid = pid1,
                            method = get
                           },
                     #{<<"password">> => <<"secret">>}
                    },
                    {#state{version = latest,
                            type = cred_data,
                            id = <<"CRED1">>,
                            session_pid = pid2,
                            method = get
                           },
                     #{<<"result">> => <<"error">>,
                       <<"user_msg">> => <<"Sorry, the requested data was not found">>}
                    },
                    {#state{version = latest,
                            type = logout,
                            id = undefined,
                            session_pid = pid1,
                            method = get
                           },
                     #{<<"result">> => <<"ok">>}
                    },
                    {#state{version = latest,
                            type = info,
                            id = undefined,
                            session_pid = pid1,
                            method = get
                           },
                     #{<<"version">> => <<"latest">>,
                       <<"redirect_path">> => <<"/oidc">>,
                       <<"error">> => <<"">>,
                       <<"logged_in">> => false,
                       <<"display_name">> => <<"">>,
                       <<"issuer_id">> => <<"">>,
                       <<"user_documentation">> => false,
                       <<"code_documentation">> => false}
                    },
                    {#state{version = latest,
                            type = access_token,
                            id = undefined,
                            session_pid = pid2,
                            method = get
                           },
                     #{<<"access_token">> => <<"AT2">>,
                       <<"issuer">> => <<"https://other.tts">>,
                       <<"subject">> => <<"sub">>,
                       <<"issuer_id">> => <<"ID2">>}
                    },
                    {#state{version = latest,
                            type = credential,
                            id = undefined,
                            session_pid = pid1,
                            method = get},
                     #{<<"credential_list">> => [#{<<"cred_id">> => <<"CRED1">>,
                                              <<"ctime">> => <<"now">>,
                                              <<"interface">> => <<"test">>,
                                              <<"service_id">> => <<"test1">>}]}
                    }
                   ],

        Test  = fun({State, ExpResult}, _) ->
                        io:format("Expecting ~p on state ~p~n",[ExpResult, State]),
                        {Result, req, _NewState} = watts_http_api:get_json(req, State),
                        ?assertEqual(ExpResult, jsone:decode(Result)),
                        ok
                end,
        ok = lists:foldl(Test,ok,Requests)
    after
        ok = stop_meck(Meck)
    end,
    ok.

post_json_test() ->
    {ok, Meck} = start_meck(),
    try
        ?SETCONFIG( ep_main, <<"/">>),
        ?SETCONFIG(ssl, false),
        ?SETCONFIG(hostname, "localhost"),
        ?SETCONFIG(port, 80),

        Url_v1 = <<"http://localhost/api/v1/credential_data/CRED1">>,
        Url_v2 = <<"http://localhost/api/v2/ID1/credential_data/CRED1">>,
        Requests = [
                    {#state{version = 2,
                            type = credential,
                            id = undefined,
                            json = #{service_id => <<"Service1">>},
                            session_pid = pid1,
                            method = post
                           }, {true, Url_v2} },
                    {#state{version = 1,
                            type = credential,
                            id = undefined,
                            json = #{service_id => <<"Service1">>},
                            session_pid = pid1,
                            method = post
                           }, {true, Url_v1} },
                    {#state{version = 2,
                            type = credential,
                            id = undefined,
                            json = #{service_id => <<"Service1">>},
                            session_pid = pid2,
                            method = post
                           }, false }
                   ],

        Test  = fun({State, ExpResult}, _) ->
                        io:format("Expecting ~p on state ~p~n",[ExpResult, State]),
                        {Result, req, _NewState} = watts_http_api:post_json(req, State),
                        ?assertEqual(ExpResult, Result),
                        ok
                end,
        ok = lists:foldl(Test,ok,Requests),
        ?UNSETCONFIG( ep_main),
        ?UNSETCONFIG( ssl),
        ?UNSETCONFIG( hostname),
        ?UNSETCONFIG( port)
    after
        ok = stop_meck(Meck)
    end,
    ok.

delete_resource_test() ->
    {ok, Meck} = start_meck(),
    try
        Requests = [
                    {#state{version = latest,
                            type = credential,
                            id = <<"CRED1">>,
                            session_pid = pid1,
                            method = delete
                           }, true },
                    {#state{version = latest,
                            type = credential,
                            id = <<"CRED1">>,
                            session_pid = pid2,
                            method = delete
                           }, false }
                   ],

        Test  = fun({State, ExpResult}, _) ->
                        io:format("Expecting ~p on state ~p~n",[ExpResult, State]),
                        {Result, req, _NewState} = watts_http_api:delete_resource(req, State),
                        ?assertEqual(ExpResult, Result),
                        ok
                end,
        ok = lists:foldl(Test,ok,Requests)
    after
        ok = stop_meck(Meck)
    end,
    ok.


start_meck() ->
    MeckModules = [cowboy_req, oidcc, watts, watts_session],
    Header = fun(Name, #{header := Header} = Request) ->
                      case lists:keyfind(Name, 1, Header) of
                          {Name, Value} -> {Value, Request};
                          _ -> {undefined, Request}
                      end;
                (_, Req) ->
                     {undefined, Req}
             end,
    ParseHeader = fun(Name, #{header := Hdr} = Request) ->
                      case lists:keyfind(Name, 1, Hdr) of
                          {Name, {Res, Value}} -> {Res, Value, Request};
                          _ -> {undefined, undefined, Request}
                      end
             end,
    Method = fun(#{method := Method} = Request) ->
                     {Method, Request}
             end,
    Body = fun(#{body := Body} = Request) ->
                     {ok, Body, Request}
             end,

    PathInfo = fun(#{path_info := PathInfo} = Request) ->
                     {PathInfo, Request}
             end,

    GetCookie = fun(_, Request) ->
                     {maps:get(cookie, Request, undefined), Request}
             end,
    SetHeader = fun(_Name, _Value, ReqIn) ->
                        ReqIn
                end,
    GetOidcProvider = fun(Issuer) ->
                          case Issuer of
                              <<"ID1">> ->
                                  {ok, #{issuer => ?ISSUER_URL}};
                              _ ->
                                  {error, not_found}
                          end
                  end,
    Login = fun(Token, _Issuer) ->
                           case Token of
                               <<"good1">> ->
                                   {ok, #{session_pid => pid1}};
                               <<"good2">> ->
                                   {ok, #{session_pid => pid2}};
                               _ ->
                                   {error, bad_token}
                           end
                   end,
    CredExists = fun(CredId, _Session) ->
                          case CredId of
                              <<"123">> -> true;
                              _ -> false
                          end
              end,
    CredDataExists = fun(DataId, _Session) ->
                          case DataId of
                              <<"someid">> -> true;
                              _ -> false
                          end
              end,

    GetProvider = fun() ->
                          {ok, [#{id => <<"ID1">>,
                                  issuer =>  <<"https://test.tts">>,
                                  ready => true
                                 },
                                #{id => <<"ID2">>,
                                  issuer => <<"https://other.tts">>,
                                  ready => false
                                 }
                               ]}
                  end,
    GetServiceList = fun(SessionPid) ->
                          case SessionPid of
                              pid1 -> {ok, [#{cred_limit => 1}]};
                              _ -> {ok, []}
                          end
                  end,
    GetCredList = fun(SessionPid) ->
                          case SessionPid of
                              pid1 -> {ok, [#{cred_id => <<"CRED1">>,
                                              ctime => <<"now">>,
                                              interface => <<"test">>,
                                              service_id => <<"test1">>,
                                              removed => true
                                             }]};
                              _ -> {ok, []}
                          end
              end,
    GetIssSub = fun(SessionPid) ->
                          case SessionPid of
                              pid1 -> {ok, <<"https://test.tts">>, <<"ID1">>,
                                            <<"sub">>};
                              pid2 -> {ok, <<"https://other.tts">>, <<"ID2">>,
                                            <<"sub">>};
                              _ -> {error, not_found}
                          end
              end,
    GetAT = fun(SessionPid) ->
                    case SessionPid of
                        pid1 -> {ok, <<"AT1">>};
                        pid2 -> {ok, <<"AT2">>};
                        _ -> {error, not_found}
                    end
            end,
    GetCred = fun(Id, SessionPid) ->
                          case {Id, SessionPid} of
                              {<<"CRED1">>, pid1} ->
                                  {ok, #{password => <<"secret">>}};
                              _ -> {error, not_found}
                          end
              end,
    Revoke = fun(CredentialId, SessionPid) ->
                     case {CredentialId, SessionPid} of
                         {<<"CRED1">>, pid1} ->
                             ok;
                         _ ->
                             {error, not_found}
                     end
             end,
    CredRequest = fun(_ServiceId, SessionPid, _Params) ->
                        case SessionPid of
                            pid1 -> {ok, #{}};
                            _ -> {error, internal}
                        end
                  end,
    StoreTempCred = fun(_Cred, SessionPid) ->
                            case SessionPid of
                                pid1 -> {ok, <<"CRED1">>};
                                pid2 -> {ok, <<"CRED2">>}
                            end
                    end,

    SessionType = fun(SessionPid) ->
                          case SessionPid of
                              pid1 -> {ok, oidc};
                              _ -> {ok, rest}
                          end
                  end,
    Peer = fun(Req) -> {{{127, 0, 0 , 1},234}, Req} end,

    IsIp = fun(Ip, _) -> Ip == {127, 0, 0, 1} end,
    IsUserAgent = fun(_, _) -> true end,

    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, body, Body),
    ok = meck:expect(cowboy_req, cookie, GetCookie),
    ok = meck:expect(cowboy_req, header, Header),
    ok = meck:expect(cowboy_req, method, Method),
    ok = meck:expect(cowboy_req, parse_header, ParseHeader),
    ok = meck:expect(cowboy_req, path_info, PathInfo),
    ok = meck:expect(cowboy_req, peer, Peer),
    ok = meck:expect(cowboy_req, set_resp_body, fun(_, Req) -> Req end),
    ok = meck:expect(cowboy_req, set_resp_cookie, fun(_, _, _, Req) -> Req end),
    ok = meck:expect(cowboy_req, set_resp_header, SetHeader),
    ok = meck:expect(watts, login_with_access_token, Login),
    ok = meck:expect(watts, does_credential_exist, CredExists),
    ok = meck:expect(watts, does_temp_cred_exist, CredDataExists),
    ok = meck:expect(watts, get_service_list_for, GetServiceList),
    ok = meck:expect(watts, get_openid_provider_list, GetProvider),
    ok = meck:expect(watts, get_openid_provider_info, GetOidcProvider),
    ok = meck:expect(watts, get_credential_list_for, GetCredList),
    ok = meck:expect(watts, get_temp_cred, GetCred),
    ok = meck:expect(watts, get_iss_id_sub_for, GetIssSub),
    ok = meck:expect(watts, get_access_token_for, GetAT),
    ok = meck:expect(watts, revoke_credential_for, Revoke),
    ok = meck:expect(watts, logout, fun(_) -> ok end),
    ok = meck:expect(watts, request_credential_for, CredRequest),
    ok = meck:expect(watts, store_temp_cred, StoreTempCred),
    ok = meck:expect(watts_session, is_logged_in, fun(_) -> false end),
    ok = meck:expect(watts_session, get_type, SessionType),
    ok = meck:expect(watts_session, is_same_ip, IsIp),
    ok = meck:expect(watts_session, is_user_agent, IsUserAgent),
    {ok, {MeckModules}}.

stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
