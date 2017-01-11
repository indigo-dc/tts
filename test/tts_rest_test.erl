-module(tts_rest_test).
-include_lib("eunit/include/eunit.hrl").
-include("tts.hrl").


-define(ISSUER_URL, <<"https://test.tts.somewhere">>).

dispatch_mapping_test() ->
    BasePath1 = <<"/test">>,
    BasePath2 = <<"/test/">>,

    ExpMapping = <<"/test/:version/:type/[:id]">>,

    Mapping1 = watts_rest:dispatch_mapping(BasePath1),
    Mapping2 = watts_rest:dispatch_mapping(BasePath2),
    ?assertEqual(ExpMapping, Mapping1),
    ?assertEqual(Mapping1, Mapping2),
    ok.


init_test() ->
    ?assertEqual({upgrade, protocol, cowboy_rest}, watts_rest:init(a,b,c)),
    ok.

%% state from tts_rest
-record(state, {
          method = undefined,
          version = undefined,
          type = undefined,
          id = undefined,

          token = undefined,
          issuer = undefined,
          json = undefined,
          session_pid = undefined,
          cookie_based = false
         }).

rest_init_test() ->
    {ok, Meck} = start_meck(),
    Req = req,
    {ok, Req, #state{}} = watts_rest:rest_init(Req, doesnt_matter),
    ok = stop_meck(Meck),
    ok.

allowed_methods_test() ->
    State = #state{},
    Req = req,
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State} =
    watts_rest:allowed_methods(Req, State),
    ok.

allow_missing_post_test() ->
    State = #state{},
    Req = req,
    {false, Req, State} = watts_rest:allow_missing_post(Req, State).

content_types_provided_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = watts_rest:content_types_provided(Req, State),
    {{<<"application">>, <<"json">>, '*'}, get_json} = ContentType,
    ok.

content_types_accepted_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = watts_rest:content_types_accepted(Req, State),
    {{<<"application">>, <<"json">>, '*'}, post_json} = ContentType,
    ok.

malformed_request_test() ->
    {ok, Meck} = start_meck(),
    State = #state{},
    Requests = [
                % GOOD request
                {#{ bindings => [{version, <<"v1">>},
                                 {type, <<"oidcp">>}
                                ],
                    header => [],
                    method => <<"GET">>,
                    body => []
                  }, false },

                { #{ bindings => [{version, <<"V1">>},
                                  {type, <<"service">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, false},

                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, false},

                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential_data">>},
                                  {id, <<"234">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, false },

                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>}
                                 ],
                     header => [{<<"content-type">>,{ok, {<<"application">>,
                                                           <<"json">>,[]}}}],
                     method => <<"POST">>,
                     body => <<"{\"service_id\":\"234\"}">>
                   }, false },
                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>},
                                  {id, <<"234">>}
                                 ],
                     header => [
                                {<<"authorization">>,<<"Bearer SomeToken">>},
                                {<<"x-openid-connect-issuer">>,?ISSUER_URL}
                               ],
                     method => <<"DELETE">>,
                     body => []
                   }, false },
                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>},
                                  {id, <<"234">>}
                                 ],
                     header => [
                                {<<"authorization">>,<<"Bearer SomeToken">>},
                                {<<"x-openid-connect-issuer">>,<<"ID1">>}
                               ],
                     method => <<"DELETE">>,
                     body => []
                   }, false },

                % BAD requests
                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>}
                                 ],
                     header => [],
                     method => <<"POST">>,
                     body => <<"{\"service_id\":234}">>
                   }, true },
                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"oidcp">>}
                                 ],
                     header => [],
                     method => <<"POST">>,
                     body => <<"no json">>
                   }, true },

                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"unknown_type">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, true },

                { #{ bindings => [{version, <<"v0">>},
                                  {type, <<"oidcp">>}
                                 ],
                     header => [{<<"authorization">>, <<"missingBearer">>}],
                     method => <<"GET">>,
                     body => []
                   }, true },

                { #{ bindings => [{version, <<"v0">>},
                                  {type, <<"credentials">>}
                                 ],
                     header => [
                                {<<"authorization">>,<<"Bearer SomeToken">>},
                                {<<"x-openid-connect-issuer">>,<<"NO URL">>}
                               ],
                     method => <<"GET">>,
                     body => []
                   }, true },

                { #{ bindings => [{version, <<"v0">>},
                                  {type, <<"credentials">>}
                                 ],
                     header => [
                                {<<"authorization">>,<<"Bearer SomeToken">>},
                                {<<"x-openid-connect-issuer">>,<<"ID2">>}
                               ],
                     method => <<"GET">>,
                     body => []
                   }, true },

                { #{ bindings => [{version, <<"vn">>},
                                  {type, <<"oidcp">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, true },

                { #{ bindings => [{version, <<"234">>},
                                  {type, <<"oidcp">>}
                                 ],
                     header => [],
                     method => <<"GET">>,
                     body => []
                   }, true }
               ],

    Test  = fun({Request, ExpResult}, _) ->
                    io:format("testing with request ~p~n",[Request]),
                    {Result, Request, _} = watts_rest:malformed_request(Request,
                                                                      State),
                    io:format("got result ~p, expecting ~p~n",[Result, ExpResult]),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = stop_meck(Meck),
    ok.

is_authorized_test() ->
    {ok, Meck} = start_meck(),
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
                   {Result, Req, _CState} = watts_rest:is_authorized(Req, State),
                   io:format("got result ~p, expecting ~p~n",[Result, ExpResult]),
                   ?assertEqual(ExpResult, Result),
                   ok
           end,
    ok = lists:foldl(Test, ok, Mapping),
    ok = stop_meck(Meck),
    ok.


resource_exists_test() ->
    {ok, Meck} = start_meck(),

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
                    {Result, req, _} = watts_rest:resource_exists(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = stop_meck(Meck),
    ok.

get_json_test() ->
    {ok, Meck} = start_meck(),
    Requests = [
                {#state{version = latest,
                        type = service,
                        id = undefined,
                        session_pid = pid1,
                        method = get
                       }, <<"{\"service_list\":[{}]}">> },
                {#state{version = latest,
                        type = service,
                        id = undefined,
                        session_pid = pid2,
                        method = get
                       }, <<"{\"service_list\":[]}">> },
                {#state{version = latest,
                        type = oidcp,
                        id = undefined,
                        session_pid = undefined,
                        method = get
                       },<<"{\"openid_provider_list\":[{\"id\":\"ID1\",\"issuer\":\"https:\\/\\/test.tts\",\"ready\":true},{\"id\":\"ID2\",\"issuer\":\"https:\\/\\/other.tts\",\"ready\":false}]}">>
                },
                {#state{version = latest,
                        type = cred_data,
                        id = <<"CRED1">>,
                        session_pid = pid1,
                        method = get
                       },<<"{\"password\":\"secret\"}">> },
                {#state{version = latest,
                        type = cred_data,
                        id = <<"CRED1">>,
                        session_pid = pid2,
                        method = get
                       },<<"{\"result\":\"error\",\"user_msg\":\"Sorry, the requested data was not found\"}">> }
               ],

    Test  = fun({State, ExpResult}, _) ->
                    io:format("Expecting ~p on state ~p~n",[ExpResult, State]),
                    {Result, req, _NewState} = watts_rest:get_json(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),

    ok = stop_meck(Meck),
    ok.

post_json_test() ->
    {ok, Meck} = start_meck(),
    ?SETCONFIG( ep_main, <<"/">>),

    Url = <<"/api/v2/credential_data/CRED1">>,
    Requests = [
                {#state{version = 2,
                        type = credential,
                        id = undefined,
                        json = #{service_id => <<"Service1">>},
                        session_pid = pid1,
                        method = post
                       }, {true, Url} },
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
                    {Result, req, _NewState} = watts_rest:post_json(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ?UNSETCONFIG( ep_main),
    ok = stop_meck(Meck),
    ok.

delete_resource_test() ->
    {ok, Meck} = start_meck(),

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
                    {Result, req, _NewState} = watts_rest:delete_resource(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = stop_meck(Meck),
    ok.


start_meck() ->
    MeckModules = [cowboy_req, oidcc, tts, tts_session],
    Binding = fun(Name, #{bindings := Bindings} = Request, Default) ->
                      case lists:keyfind(Name, 1, Bindings) of
                          {Name, Value} -> {Value, Request};
                          _ -> {Default, Request}
                      end
              end,
    Binding2 = fun(Name, Request) ->
                       Binding(Name, Request, undefined)
               end,
    Header = fun(Name, #{header := Header} = Request) ->
                      case lists:keyfind(Name, 1, Header) of
                          {Name, Value} -> {Value, Request};
                          _ -> {undefined, Request}
                      end
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
                              pid1 -> {ok, [#{}]};
                              _ -> {ok, []}
                          end
                  end,
    GetCredList = fun(SessionPid) ->
                          case SessionPid of
                              pid1 -> {ok, [<<"CredId">>]};
                              _ -> {ok, []}
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
    CredRequest = fun(_ServiceId, SessionPid, _Params, _IFace) ->
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

    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, binding, Binding),
    ok = meck:expect(cowboy_req, binding, Binding2),
    ok = meck:expect(cowboy_req, cookie, GetCookie),
    ok = meck:expect(cowboy_req, header, Header),
    ok = meck:expect(cowboy_req, parse_header, ParseHeader),
    ok = meck:expect(cowboy_req, method, Method),
    ok = meck:expect(cowboy_req, body, Body),
    ok = meck:expect(cowboy_req, set_resp_body, fun(_, Req) -> Req end),
    ok = meck:expect(cowboy_req, set_resp_header, SetHeader),
    ok = meck:expect(oidcc, get_openid_provider_info, GetOidcProvider),
    ok = meck:expect(tts, login_with_access_token, Login),
    ok = meck:expect(tts, does_credential_exist, CredExists),
    ok = meck:expect(tts, does_temp_cred_exist, CredDataExists),
    ok = meck:expect(tts, get_service_list_for, GetServiceList),
    ok = meck:expect(tts, get_openid_provider_list, GetProvider),
    ok = meck:expect(tts, get_credential_list_for, GetCredList),
    ok = meck:expect(tts, get_temp_cred, GetCred),
    ok = meck:expect(tts, revoke_credential_for, Revoke),
    ok = meck:expect(tts, logout, fun(_) -> ok end),
    ok = meck:expect(tts, request_credential_for, CredRequest),
    ok = meck:expect(tts, store_temp_cred, StoreTempCred),
    ok = meck:expect(tts_session, is_logged_in, fun(_) -> false end),
    {ok, {MeckModules}}.

stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
