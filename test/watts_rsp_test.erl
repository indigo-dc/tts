-module(watts_rsp_test).
-include_lib("eunit/include/eunit.hrl").
-include("watts.hrl").

-define(RSP1, #{id => <<"id1">>,
                key_location => <<"file:///doesnotexist">>,
                show_ui => false,
                perform_login => false,
                base_url => <<"http://rsp.watts.kit.edu">>
               }).

-define(RSP2, #{id => <<"id2">>,
                key_location => <<"file:///doesnotexist">>,
                show_ui => false,
                perform_login => true,
                base_url => <<"https://rsp.watts.kit.edu">>
               }).

new_test() ->
    {ok, Meck} = start_meck(),
    try
        Id = <<"test">>,
        Config = #{id => Id,
                   key_location => <<"loc">>,
                   show_ui => true,
                   perform_login => true,
                   base_url => <<"https://rsp.watts.kit.edu">>},
        {ok, Rsp} = watts_rsp:new(Config),
        {ok, Id} = watts_rsp:get_id(Rsp),
        {rsp, ui, login} = watts_rsp:session_type(Rsp)
    after
        stop_meck(Meck)
    end,
    ok.

exists_test() ->
    {ok, Meck} = start_meck(),
    {ok, R} = watts_rsp:new(?RSP1),
    application:set_env(watts, rsp_list, [R]),
    try
        true = watts_rsp:exists(<<"id1">>),
        false = watts_rsp:exists(<<"id2">>),
        Rsp1 = watts_rsp:get_info(<<"id1">>),
        Rsp2 = watts_rsp:get_info(<<"id2">>),
        {ok, <<"id1">>} = watts_rsp:get_id(Rsp1),
        {rsp, no_ui, no_login} = watts_rsp:session_type(Rsp1),
        {ok, undefined} = watts_rsp:get_id(Rsp2)
    after
        stop_meck(Meck)
    end,
    ok.

validate_jwt_test() ->
    {ok, Meck} = start_meck(),
    {ok, R} = watts_rsp:new(?RSP2),
    application:set_env(watts, rsp_list, [R]),
    try
        GoodUrl = <<"https://rsp.watts.kit.edu/cool">>,
        BadUrl = <<"https://rsp.watts.kit.edu/bad">>,
        Iss0 = <<"id2">>,
        Prefix = <<"rsp-">>,
        Iss = << Prefix/binary, Iss0/binary>>,
        Sub = <<"joe">>,
        Service = <<"rsp-service">>,
        Claims = #{iss => Iss0,
                   sub =>  Sub,
                   exp => now(10),
                   iat => now(0),
                   watts_service => Service,
                   watts_params => [{<<"one">>, true}, {<<"name">>, <<"john">>}],
                   watts_provider =>  <<"iam">>,
                   success_url => GoodUrl,
                   failed_url => BadUrl
                  },
        JwtMap = #{claims => Claims, header => #{typ => <<"watts-rsp">>}},
        Referer = <<"https://rsp.watts.kit.edu/referer">>,
        {ok, Rsp} = watts_rsp:validate_jwt_get_rsp(JwtMap, Referer),
        rsp_no_ui_login = watts_rsp:request_type(Rsp),
        <<"iam">> = watts_rsp:get_provider(Rsp),
        {Iss, Sub} = watts_rsp:get_iss_sub(Rsp),
        {Service, #{ <<"one">> := true, <<"name">> := <<"john">>}} = watts_rsp:get_service_data(Rsp),
        {GoodUrl, BadUrl} = watts_rsp:get_return_urls(Rsp)
    after
        stop_meck(Meck)
    end,
    ok.

validate_jwt_fail_test() ->
    {ok, Meck} = start_meck(),
    {ok, R} = watts_rsp:new(?RSP1),
    application:set_env(watts, rsp_list, [R]),
    try
        GoodUrl = <<"http://rsp.watts.kit.edu/cool">>,
        BadUrl = <<"http://rsp.watts.kit.edu/bad">>,
        Iss = <<"id1">>,
        Sub = <<"joe">>,
        Service = <<"rsp-service">>,
        Claims = #{iss => Iss,
                   sub =>  Sub,
                   exp => now(10),
                   iat => now(0),
                   watts_service => Service,
                   watts_params => #{<<"one">> => true, <<"name">>  => <<"john">>},
                   watts_provider =>  <<"iam">>,
                   success_url => GoodUrl,
                   failed_url => BadUrl
                  },
        JwtMap = #{claims => Claims},
        Referer = <<"http://rsp.watts.kit.edu/referer">>,
        {error, {bad_jwt, _}, BadUrl} = watts_rsp:validate_jwt_get_rsp(JwtMap, Referer)
    after
        stop_meck(Meck)
    end,
    ok.

start_meck() ->
    MeckModules = [watts_rsp_keys, erljwt, watts_service, oidcc],

    New =  fun(_) ->
                   {ok, self()}
           end,
    ToMap = fun(In) ->
                    {ok, In}
            end,
    GetKeys = fun(_) ->
                      {ok, keys}
              end,
    Validate = fun(#{claims := #{ iss := Iss}} = Jwt, _, _, _) ->
                       case Iss of
                           <<"id2">> -> {ok, Jwt};
                           _ -> {error, no_key}
                       end
               end,
    ok = meck:expect(erljwt, to_map, ToMap),
    ok = meck:expect(erljwt, validate, Validate),
    ok = meck:expect(watts_service, exists, fun(_) -> true end),
    ok = meck:expect(oidcc, get_openid_provider_info, fun(_) -> {ok, whatever} end),
    ok = meck:expect(watts_rsp_keys, new, New),
    ok = meck:expect(watts_rsp_keys, get_keys, GetKeys),
    {ok, MeckModules}.


stop_meck(MeckModules) ->
    application:set_env(watts, rsp_list, []),
    ok = test_util:meck_done(MeckModules).

now(Seconds) ->
    erlang:system_time(seconds) + Seconds.
