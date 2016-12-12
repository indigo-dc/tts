-module(rest_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0,
         %% groups/0,
         %% group/1,
         %% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         %% init_per_group/2,
         %% end_per_group/2,
         %% init_per_testcase/2,
         %% end_per_testcase/2,

         python2_check/1,
         service_config/1,
         provider_config/1,
         init_done/1,
         %% provider_list/0,
         rest_communication/1
        ]).

all() ->
    [
     python2_check,
     provider_config,
     service_config,
     init_done,
     rest_communication
    ].

%% groups() ->
%%     [].
%%
%% group(_) ->
%%     [].
%%
%% suite() ->
%%     [].

init_per_suite(Conf) ->
    ServiceList = [#{allow_same_state => false,
                  authz => #{allow => [{any,<<"sub">>,any,true}],
                    forbid => [],
                    hide => false,
                    tooltip => <<"Tooltip">>},
                  cmd => <<"/tmp/tts_common_test/tts_plugin_info/plugin/info.py">>,
                  connection => #{host => undefined,
                    passwd => undefined,
                    port => 22,
                    ssh_dir => undefined,
                    ssh_key_pass => undefined,
                    type => local,
                    user => undefined},
                  cred_limit => 1,
                  description => <<"Simple Info Service">>,
                  id => <<"info">>,
                  parallel_runner => 1,
                  pass_access_token => false,
                  plugin_conf_config => #{},
                  plugin_timeout => 10000}],
    ProviderList = [#{client_id => <<"1234">>,
                   client_secret => <<"secret">>,
                   config_endpoint => <<"https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration">>,
                   description => <<"INDIGO IAM">>,
                   id => <<"iam">>,
                   scopes => [<<"openid">>,<<"profile">>]}],
    application:set_env(tts, service_list, ServiceList),
    application:set_env(tts, provider_list, ProviderList),
    application:set_env(tts, session_timeout, 90000),
    application:set_env(tts, sqlite_db, "/tmp/tts_common_test/tts_ct.db"),
    application:set_env(tts, allow_dropping_credentials, true),
    application:set_env(tts, redirection_enabled, false),
    application:set_env(tts, redirection_port, 8080),
    application:set_env(tts, ssl, false),
    application:set_env(tts, port, 8080),
    application:set_env(tts, listen_port, 8080),
    application:set_env(tts, ep_main, <<"/">>),
    application:set_env(tts, hostname, "localhost"),
    {ok, _} = application:ensure_all_started(tts),
    Conf.

end_per_suite(Conf) ->
    ok = application:stop(tts),
    Conf.

%% init_per_group(_Group, Conf) ->
%%     Conf.
%%
%% end_per_group(_Group, Conf) ->
%%     Conf.
%%
%% init_per_testcase(_TestCase, Conf) ->
%%      mock_oidcc(),
%%      Conf.
%%
%% end_per_testcase(_TestCase, Conf) ->
%%     unmock_oidcc(),
%%     Conf.


service_config(_Config) ->
    {ok, Services} = tts_service:get_list(),
    ct:log("service configs:~n~p~n", [Services]),
    ok.

provider_config(_Config) ->
    WaitList = fun() ->
                   {ok, Provider} = tts:get_openid_provider_list(),
                   length(Provider) >= 1
           end,
    ok = test_util:wait_for_true(WaitList, 10),
    WaitReady = fun() ->
                   {ok, Provider} = tts:get_openid_provider_list(),
                   [#{ready := Result}| _] = Provider,
                   Result
           end,
    ok = test_util:wait_for_true(WaitReady, 1000),
    {ok, Provider} = tts:get_openid_provider_list(),
    ct:log("provider configs:~n~p~n", [Provider]),
    ok.

init_done(_Config) ->
    Done = fun() ->
                   Children = supervisor:which_children(tts_sup),
                   case lists:keyfind(init, 1, Children) of
                       {init, undefined, worker, _} ->
                           true;
                       _ ->
                           false
                   end
           end,
    ok = test_util:wait_for_true(Done, 100),
    ok.

python2_check(_Config) ->
    Result = os:cmd("env python2 --version"),
    ct:log("python2 check result ~p", [Result]),
    [H | _] = binary:split(list_to_binary(Result),[<<":">>,<<" ">>],[global,trim_all]),
    ok = case H of
             <<"Python">> -> ok;
             _ -> no_python
         end,
    ok.



rest_communication(_Config) ->
    mock_oidcc(),
    Issuer = <<"https://iam-test.indigo-datacloud.eu/">>,

    {ok, ProviderList} = perform_rest_request("lsprov"),
    {ok, _ProvId} = validate_provider_list(ProviderList, Issuer),
    {ok, ServiceList} = perform_rest_request("lsserv"),
    {ok, ServiceId} = validate_service_list(list_to_binary(ServiceList)),
    {ok, CredList} = perform_rest_request("lscred"),
    true = is_empty_credential_list(CredList),
    {ok, CredentialData} = perform_rest_request("request "++ServiceId),
    {ok, CredId} = validate_credential(CredentialData),
    {ok, CredList2} = perform_rest_request("lscred"),
    false = is_empty_credential_list(CredList2),
    {ok, _Result} = perform_rest_request("revoke "++CredId),
    {ok, CredList3} = perform_rest_request("lscred"),
    true = is_empty_credential_list(CredList3),
    unmock_oidcc(),
    ok.

validate_provider_list(String, Issuer) when is_list(String) ->
    validate_provider_list(list_to_binary(String), Issuer);
validate_provider_list(Bin, Issuer) ->
    Entries = binary:split(Bin, [<<"[">>, <<"]">>, <<" (">>, <<")\n">>],
                           [global, trim_all]),
    ct:log("provider list entries: ~p",[Entries]),
    find_provider(Entries, Issuer).

find_provider([], _Issuer) ->
    {error, not_found};
find_provider([Id, <<"ready">>, _Desc, Issuer | _T], Issuer) ->
    {ok, binary_to_list(Id)};
find_provider([_H | T], Issuer) ->
    find_provider(T, Issuer).

validate_service_list(Bin) ->
    Entries = binary:split(Bin, [<<"[">>, <<"]">>], [global, trim_all]),
    ct:log("service list entries: ~p",[Entries]),
    [_, Id | _ ] = Entries,
    {ok, binary_to_list(Id)}.

is_empty_credential_list(Output) when is_list(Output) ->
    is_empty_credential_list(list_to_binary(Output));
is_empty_credential_list(Bin) ->
    case binary:match(Bin, <<"*** no credentials ***">>) of
        nomatch ->
            false;
        _ ->
            true
    end.


validate_credential(Data) when is_list(Data) ->
    validate_credential(list_to_binary(Data));
validate_credential(Data) ->
    Entries = binary:split(Data, [<<"[">>, <<"]">>], [global, trim_all]),
    ct:log("credential enries: ~p~n", [Entries]),
    [_, _, _, Id | _ ] = Entries,
    {ok, binary_to_list(Id)}.










%%%%%% internal
perform_rest_request(Params) ->
    Exec = "/tmp/tts_common_test/ttsc/ttsc",
    Cmd = "export TTSC_TOKEN=MockToken && export TTSC_ISSUER=https://iam-test.indigo-datacloud.eu/ && export TTSC_URL=http://localhost:8080 && "++Exec++" "++Params,
    Result = os:cmd(Cmd),
    ct:log("executed '~s ~s' with result ~p~n",[Exec, Params, Result]),
    {ok, Result}.


mock_oidcc() ->
    Info = #{sub => <<"tts_test_user">>, name => <<"joe">>},
    meck:new(oidcc, [passthrough]),
    UserInfo2 = fun(Token, ProviderId) ->
                       ct:log("Returning UserInfo ~p for request to provider ~p
                       with token ~p~n",[Info, ProviderId, Token]),
                       {ok, Info}
               end,
    UserInfo3 = fun(Token, ProviderId, _Subject) ->
                       ct:log("Returning UserInfo ~p for request to provider ~p
                       with token ~p~n",[Info, ProviderId, Token]),
                       {ok, Info}
               end,
    meck:expect(oidcc, retrieve_user_info, UserInfo2),
    meck:expect(oidcc, retrieve_user_info, UserInfo3),
    ok.

unmock_oidcc() ->
    true = meck:validate(oidcc),
    ok = meck:unload(oidcc),
    ok.
