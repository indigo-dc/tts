-module(tts_rest_test).
-include_lib("eunit/include/eunit.hrl").


dispatch_mapping_test() ->
    BasePath1 = <<"/test">>,
    BasePath2 = <<"/test/">>,

    ExpMapping = <<"/test/[:version]/:type/[:id]">>,

    Mapping1 = tts_rest:dispatch_mapping(BasePath1),
    Mapping2 = tts_rest:dispatch_mapping(BasePath2),
    ?assertEqual(ExpMapping, Mapping1),
    ?assertEqual(Mapping1, Mapping2),
    ok.


init_test() ->
    ?assertEqual({upgrade, protocol, cowboy_rest}, tts_rest:init(a,b,c)),
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
          session_pid = undefined
         }).

rest_init_test() ->
    MeckModules = [cowboy_req],
    SetHeader = fun(_Name, _Value, ReqIn) ->
                        ReqIn
                end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, set_resp_header, SetHeader),
    Req = req,
    {ok, Req, #state{}} = tts_rest:rest_init(Req, doesnt_matter),
    ok = test_util:meck_done(MeckModules),
    ok.

allowed_methods_test() ->
    State = #state{},
    Req = req,
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State} =
    tts_rest:allowed_methods(Req, State),
    ok.

allow_missing_post_test() ->
    State = #state{},
    Req = req,
    {false, Req, State} = tts_rest:allow_missing_post(Req, State).

content_types_provided_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = tts_rest:content_types_provided(Req, State),
    {{<<"application">>, <<"json">>, '*'}, get_json} = ContentType,
    ok.

content_types_accepted_test() ->
    State = #state{},
    Req = req,
    {[ContentType], Req, State} = tts_rest:content_types_accepted(Req, State),
    {{<<"application">>, <<"json">>, '*'}, post_json} = ContentType,
    ok.

malformed_request_test() ->
    MeckModules = [cowboy_req, oidcc],
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
    Method = fun(#{method := Method} = Request) ->
                     {Method, Request}
             end,
    Body = fun(#{body := Body} = Request) ->
                     {ok, Body, Request}
             end,
    IssuerUrl = <<"https://test.tts.somewhere">>,
    GetProvider = fun(Issuer) ->
                          case Issuer of
                              <<"ID1">> ->
                                  {ok, #{issuer => IssuerUrl}};
                              _ ->
                                  {error, not_found}
                          end
                  end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(cowboy_req, binding, Binding),
    ok = meck:expect(cowboy_req, binding, Binding2),
    ok = meck:expect(cowboy_req, header, Header),
    ok = meck:expect(cowboy_req, method, Method),
    ok = meck:expect(cowboy_req, body, Body),
    ok = meck:expect(oidcc, get_openid_provider_info, GetProvider),

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
                     header => [],
                     method => <<"POST">>,
                     body => <<"{\"service_id\":234}">>
                   }, false },
                { #{ bindings => [{version, <<"latest">>},
                                  {type, <<"credential">>},
                                  {id, <<"234">>}
                                 ],
                     header => [
                                {<<"authorization">>,<<"Bearer SomeToken">>},
                                {<<"x-openid-connect-issuer">>,IssuerUrl}
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
                    {Result, Request, _} = tts_rest:malformed_request(Request,
                                                                      State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = test_util:meck_done(MeckModules),
    ok.

is_authorized_test() ->
    Mapping = [

               {#state{}, {false, <<"Authorization">>}},
               {#state{type=some_unknown, token=defined}, {false, <<"Authorization">>}},
               {#state{type=service}, {false, <<"Authorization">>}},
               {#state{type=service, issuer= <<"issuer">>}, {false, <<"Authorization">>}},
               {#state{type=service, issuer= <<"issuer">>, token= <<"token">>}, {false, <<"Authorization">>}},
               {#state{type=some_service, issuer= <<"issuer">>, token= <<"token">>}, {false, <<"Authorization">>}},

               {#state{type=oidcp}, true},
               {#state{type=service, issuer= <<"issuer">>, token= <<"token">>}, {false, <<"Authorization">>}},
               {#state{type=service, issuer= <<"unknown">>, token= <<"token">>}, {false, <<"Authorization">>}},
               {#state{type=service, issuer= <<"known">>, token= <<"token">>}, {false, <<"Authorization">>}},
               {#state{type=service, issuer= <<"known">>, token= <<"good1">>}, true},
               {#state{type=service, issuer= <<"known">>, token= <<"good2">>}, true}
              ],
    Req = req,
    MeckModules = [tts],

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

    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts, login_with_access_token, Login),

    Test = fun({State, ExpResult}, _AccIn) ->
                   {Result, Req, _CState} = tts_rest:is_authorized(Req, State),
                   ?assertEqual(ExpResult, Result),
                   ok
           end,
    ok = lists:foldl(Test, ok, Mapping),
    ok = test_util:meck_done(MeckModules),
    ok.


resource_exists_test() ->
    MeckModules = [tts],

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


    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts, does_credential_exist, CredExists),
    ok = meck:expect(tts, does_temp_cred_exist, CredDataExists),

    Requests = [
                %% good requests
                {#state{id = undefined}, true},
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
                {#state{type = creddata, id= <<"someid">>}, false}

               ],

    Test  = fun({State, ExpResult}, _) ->
                    io:format("testing ~p, expecting: ~p~n", [State, ExpResult]),
                    {Result, req, _} = tts_rest:resource_exists(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = test_util:meck_done(MeckModules),
    ok.

get_json_test() ->
    MeckModules = [tts],

    GetProvider = fun() ->
                          {ok, [{<<"ID1">>,pid}]}
                  end,
    GetPInfo = fun(_Id) ->
                          {ok, #{issuer => <<"https://test.tts">>, ready => true}}
                  end,
    GetServiceList = fun(UserId) ->
                          case UserId of
                              <<"123">> -> {ok, [#{}]};
                              _ -> {ok, []}
                          end
                  end,
    GetCList = fun(UserId) ->
                          case UserId of
                              <<"123">> -> {ok, [<<"CredId">>]};
                              _ -> {ok, []}
                          end
              end,
    GetCred = fun(Id, UserId) ->
                          case {Id, UserId} of
                              {<<"CRED1">>, <<"123">>} ->
                                  {ok, #{password => <<"secret">>}};
                              _ -> {error, not_found}
                          end
              end,
    ok = test_util:meck_new(MeckModules),
    ok = meck:expect(tts_service, get_list, GetServiceList),
    ok = meck:expect(oidcc, get_openid_provider_list, GetProvider),
    ok = meck:expect(oidcc, get_openid_provider_info, GetPInfo),
    ok = meck:expect(tts_credential, get_list, GetCList),
    ok = meck:expect(tts_rest_cred, get_cred, GetCred),

    Requests = [
                {#state{version = latest,
                        type = service,
                        id = undefined,
                        user_info = #{ site => #{uid => <<"123">>}},
                        method = get
                       }, <<"{\"service_list\":[{}]}">> },
                {#state{version = latest,
                        type = service,
                        id = undefined,
                        user_info = #{ site => #{uid => <<"124">>}},
                        method = get
                       }, <<"{\"service_list\":[]}">> },
                {#state{version = latest,
                        type = oidcp,
                        id = undefined,
                        user_info = undefined,
                        method = get
                       },<<"{\"openid_provider_list\":[{\"id\":\"ID1\",\"issuer\":\"https://test.tts\"}]}">>
                },
                {#state{version = latest,
                        type = credential,
                        id = undefined,
                        user_info = #{ site => #{uid => <<"123">>}},
                        method = get
                       },<<"{\"credential_list\":[{\"id\":\"CredId\"}]}">> },
                {#state{version = latest,
                        type = cred_data,
                        id = <<"CRED1">>,
                        user_info = #{ site => #{uid => <<"123">>}},
                        method = get
                       },<<"{\"password\":\"secret\"}">> },
                {#state{version = latest,
                        type = cred_data,
                        id = <<"CRED1">>,
                        user_info = #{ site => #{uid => <<"124">>}},
                        method = get
                       },<<"{}">> }
               ],

    Test  = fun({State, ExpResult}, _) ->
                    {Result, req, State} = tts_rest:get_json(req, State),
                    ?assertEqual(ExpResult, Result),
                    ok
            end,
    ok = lists:foldl(Test,ok,Requests),
    ok = test_util:meck_done(MeckModules),

    ok.

%% post_json_test() ->
%%     MeckModules = [ tts_credential, tts_rest_cred],
%%     CredRequest = fun(_ServiceId, #{ site := #{uid := Uid}}, _IFace, rest, []) ->
%%                         case Uid of
%%                             <<"123">> -> {ok, #{}, []};
%%                             _ -> {error, internal}
%%                         end
%%                   end,
%%     AddCred = fun(_Cred, UserId) ->
%%                       case UserId of
%%                           <<"123">> -> {ok, <<"CRED1">>};
%%                           _ -> {error, internal}
%%                       end
%%               end,

%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_credential, request, CredRequest),
%%     ok = meck:expect(tts_rest_cred, add_cred, AddCred),

%%     Url = <<"/api/v1/credential_data/CRED1">>,
%%     Requests = [
%%                 {#state{version = 1,
%%                         type = credential,
%%                         id = undefined,
%%                         json = #{service_id => <<"Service1">>},
%%                         user_info = #{ site => #{uid => <<"123">>}},
%%                         method = post
%%                        }, {true, Url} },
%%                 {#state{version = 1,
%%                         type = credential,
%%                         id = undefined,
%%                         json = #{service_id => <<"Service1">>},
%%                         user_info = #{ site => #{uid => <<"124">>}},
%%                         method = post
%%                        }, false }
%%                ],

%%     Test  = fun({State, ExpResult}, _) ->
%%                     {Result, req, State} = tts_rest:post_json(req, State),
%%                     ?assertEqual(ExpResult, Result),
%%                     ok
%%             end,
%%     ok = lists:foldl(Test,ok,Requests),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.

%% delete_resource_test() ->
%%     MeckModules = [tts_credential],
%%     Revoke = fun(CredentialId, #{ site := #{uid := Uid}}) ->
%%                      case {CredentialId, Uid} of
%%                          {<<"CRED1">>, <<"123">>} ->
%%                              {ok, result, []};
%%                          _ ->
%%                              {error, not_found}
%%                      end
%%              end,
%%     ok = test_util:meck_new(MeckModules),
%%     ok = meck:expect(tts_credential, revoke, Revoke),

%%     Requests = [
%%                 {#state{version = latest,
%%                         type = credential,
%%                         id = <<"CRED1">>,
%%                         user_info = #{ site => #{uid => <<"123">>}},
%%                         method = delete
%%                        }, true },
%%                 {#state{version = latest,
%%                         type = credential,
%%                         id = <<"CRED1">>,
%%                         user_info = #{ site => #{uid => <<"124">>}},
%%                         method = delete
%%                        }, false }
%%                ],

%%     Test  = fun({State, ExpResult}, _) ->
%%                     {Result, req, State} = tts_rest:delete_resource(req, State),
%%                     ?assertEqual(ExpResult, Result),
%%                     ok
%%             end,
%%     ok = lists:foldl(Test,ok,Requests),
%%     ok = test_util:meck_done(MeckModules),
%%     ok.
