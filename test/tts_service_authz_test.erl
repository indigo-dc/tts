-module(tts_service_authz_test).
-include_lib("eunit/include/eunit.hrl").

-define(ISSUER, <<"https://iam.it">>).
authorization_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"test">>,
    Sub = <<"sub">>,
    Iam = <<"iam">>,
    Other = <<"iom">>,

    {ok, UserInfo0} = tts_userinfo:new(),
    IdToken = #{claims => #{sub => Sub,
                            iss => ?ISSUER,
                            groups => [<<"Developer">>, <<"User">>],
                            acr => <<"https://egi.eu/LoA#Substantial">>,
                            groups2 => <<"good,bad,bad-good,not_good ">>,
                            groups3 => <<"good,bad,tbad-good,not_good ">>
                           }},
    {ok, UserInfo1} = tts_userinfo:update_id_token(IdToken
                                                 , UserInfo0),
    UserInfo = UserInfo1,
    io:format("userinfo: ~p~n",[UserInfo]),
    {ok, RegExp} = re:compile(",bad-good,|^bad-good,|,bad-good$|^bad-good$"),

    Tests = [
             {[{Iam, <<"sub">>, any, true}],true, false},
             {[{Iam, <<"sub">>, equals, <<"sub">>}],true, false},
             {[{Iam, <<"sub">>, equals, <<"sub1">>}],false, true},
             {[{Iam, <<"sub">>, is_member_of, [<<"sub1">>, <<"sub">>]}],true, false},
             {[{Iam, <<"sub">>, is_member_of, [<<"sub1">>]}],false, true},
             {[{Iam, <<"groups">>, contains, <<"Developer">>}],true, false},
             {[{Iam, <<"groups">>, contains, <<"Developers">>}],false, true},
             {[{Iam, <<"groups2">>, contains, <<"good">>}],true, false},
             {[{Iam, <<"groups2">>, contains, <<"goods">>}],false, true},
             {[{Iam, <<"groups2">>, contains, <<"goods">>}],false, true},
             {[{Iam, <<"groups2">>, regexp, RegExp}],true, false},
             {[{Iam, <<"groups3">>, regexp, RegExp}],false, true},

             {[{Iam, <<"sub">>, any, true},
               {Iam, <<"groups2">>, contains, <<"happy">>}],true, false},

             {[{Other, <<"sub">>, any, true}],false, true},
             {[{Iam, <<"no-key">>, any, true}],false, false},

             {[{Iam, <<"sub">>, no_operation, true}],false, false},

             {[],false, true}
            ],
    AllowRule = [{any,<<"sub">>,any,true}],
    CheckAuth
        = fun({Rules, Exp1, Exp2}, _) ->
                  AuthMap1 = #{allow => Rules, forbid => []},
                  AuthMap2 = #{allow => AllowRule, forbid => Rules},
                  Res1 = tts_service_authz:is_authorized(ServiceId, UserInfo,
                                                        AuthMap1),
                  Res2 = tts_service_authz:is_authorized(ServiceId, UserInfo,
                                                        AuthMap2),
                  case {Exp1 == Res1, Exp2 == Res2} of
                      {false, false} ->
                          io:format("tested ~p expected ~p got ~p~n",[AuthMap1, Exp1, Res1]),
                          io:format("tested ~p expected ~p got ~p~n",[AuthMap2, Exp2, Res2]),
                          ?assertEqual(Exp1, Res1);
                      {false, true} ->
                          io:format("tested ~p expected ~p got ~p~n",[AuthMap1, Exp1, Res1]),
                          ?assertEqual(Exp1, Res1);
                      {true, false} ->
                          io:format("tested ~p expected ~p got ~p~n",[AuthMap2, Exp2, Res2]),
                          ?assertEqual(Exp2, Res2);
                      _ ->
                          ok
                  end
          end,

    lists:foldl(CheckAuth, ignored, Tests),
    ok = stop_meck(Meck),
    ok.


validate_config_test() ->
    {ok, Meck} = start_meck(),
    ServiceId = <<"test">>,
    Entries = [
               {
                 [{<<"iam">>, <<"groups">>, contains, <<"Users">>}]
               , [{<<"iam">>, <<"groups">>, contains, <<"Users">>}]},

               {[{<<"google">>, <<"groups">>, contains, <<"Users">>}],
                 []},

               {[{any, <<"groups">>, contains, <<"Users">>}],
                [{any, <<"groups">>, contains, <<"Users">>}]},

               {[],[]}
              ],

    EntryToConfig = fun({Conf, Exp}, List) ->
                            Allow = { #{allow => Conf, forbid => []},
                                      #{allow => Exp, forbid => []}},
                            Forbid = { #{allow => [], forbid => Conf},
                                       #{allow => [], forbid => Exp}},
                            [Allow, Forbid] ++ List
                    end,
    Configs = lists:foldl(EntryToConfig, [], Entries),

    Validate
        = fun({Config, Exp}, _)  ->
                  {ok, Res} = tts_service_authz:validate_config(ServiceId,
                                                                Config),
                  ?assertEqual(Exp, Res),
                  ok
          end,
    lists:foldl(Validate, ignored, Configs),
    ok = stop_meck(Meck),
    ok.

start_meck() ->
    MeckModules = [oidcc],
    ok = test_util:meck_new(MeckModules),
    GetProviderList =
        fun() ->
                {ok, [{<<"iam">>, something}]}
        end,
    GetProviderInfo =
        fun(Id) ->
                case Id of
                    <<"iam">> ->
                        {ok, #{issuer => ?ISSUER, id => <<"iam">>}};
                    pid ->
                        {ok, #{issuer => ?ISSUER, id => <<"iam">>}};
                    _ ->
                        {error, not_found}
                end
        end,
    FindProvider = fun(Iss) ->
                           case Iss of
                               ?ISSUER ->
                                   {ok, pid};
                               _ ->
                                   {error, not_found}
                           end
                   end,

    ok = meck:expect(oidcc, find_openid_provider, FindProvider),
    ok = meck:expect(oidcc, get_openid_provider_list, GetProviderList),
    ok = meck:expect(oidcc, get_openid_provider_info, GetProviderInfo),
    {ok, {MeckModules}}.


stop_meck({MeckModules}) ->
    ok = test_util:meck_done(MeckModules),
    ok.
