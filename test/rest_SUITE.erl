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
         update_config/1,
         service_config/1,
         %% provider_list/0,
         rest_communication/1
        ]).

all() ->
    [
     python2_check,
     update_config,
     service_config,
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


update_config(_Config) ->
    ConfigDir = tts_based_dir("./test/config_ct"),
    ct:pal("config-dir: ~p",[ConfigDir]),
    tts_config:reload(ConfigDir),
    wait_for_config(),
    AppConf = application:get_all_env(tts),
    ct:pal("the configuration is: ~p",[AppConf]),
    ok = wait_for_oidcc_ready(),
    ok.

service_config(_Config) ->
    {ok, Services} = tts_service:get_list(),
    ct:log("service configs:~n~p~n", [Services]),
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

    {ok, ProviderList} = perform_rest_request("lsprov localhost:8080"),
    {ok, ProvId} = validate_provider_list(ProviderList, Issuer),
    {ok, ServiceList} = perform_rest_request("lsserv localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    {ok, ServiceList} = perform_rest_request("lsserv localhost:8080 MockToken "++ProvId),
    {ok, ServiceId} = validate_service_list(list_to_binary(ServiceList)),
    {ok, []} = perform_rest_request("lscred localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    {ok, CredentialData} = perform_rest_request("request localhost:8080 "++ServiceId++" MockToken https://iam-test.indigo-datacloud.eu/"),
    Credential = jsx:decode(list_to_binary(CredentialData), [return_maps, {labels, attempt_atom}]),
    {ok, CredId} = validate_credential(Credential),
    {ok, CredList} = perform_rest_request("lscred localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    true = validate_credential_list(list_to_binary(CredList), list_to_binary(CredId)),
    {ok, _Result} = perform_rest_request("revoke localhost:8080 "++CredId++" MockToken https://iam-test.indigo-datacloud.eu/"),
    {ok, []} = perform_rest_request("lscred localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    unmock_oidcc(),
    ok.

validate_provider_list(String, Issuer) when is_list(String) ->
    validate_provider_list(list_to_binary(String), Issuer);
validate_provider_list(Bin, Issuer) ->
    Entries = binary:split(Bin, [<<"\n">>, <<" ">>, <<"\"">>], [global, trim_all]),
    ct:log("provider list entries: ~p",[Entries]),
    find_provider(Entries, Issuer).
find_provider([Id, Issuer | _T], Issuer) ->
    {ok, binary_to_list(Id)};
find_provider([_Id, _Iss | T], Issuer) ->
    find_provider(T, Issuer).

validate_service_list(Bin) ->
    Entries = binary:split(Bin, [<<"\n">>, <<" ">>, <<"\"">>], [global, trim_all]),
    ct:log("service list entries: ~p",[Entries]),
    [Id | _ ] = Entries,
    {ok, binary_to_list(Id)}.

validate_credential([#{name := <<"id">>, type := <<"text">>, value := Id} | _T]) ->
    {ok, binary_to_list(Id)};
validate_credential([_H | T]) ->
    validate_credential(T).

validate_credential_list(Data, Id) ->
    Map = jsx:decode(Data, [return_maps, {labels, attempt_atom}]),
    case maps:get(cred_id,Map) of
        Id -> true;
        _ -> false
    end.












%%%%%% internal
perform_rest_request(Params) ->
    Script = tts_based_dir("./client/ttsc.sh"),
    Cmd = "export URI_SCHEME=http && "++Script++" "++Params,
    Result = os:cmd(Cmd),
    BinResult = list_to_binary(Result),
    ct:log("executed ~p with result ~p~n",[Cmd, Result]),
    case binary:split(BinResult,[<<" ">>]) of
        [<<"ERROR">> | _] -> {error, Result};
        _ -> {ok, Result}
    end.

wait_for_config() ->
    case tts_config:is_loaded() of
        true -> ok;
        _ -> timer:sleep(100),
             wait_for_config()
    end.

tts_based_dir(Dir) ->
    PrivDir = code:priv_dir(tts),
    [BinBaseDir | _] = binary:split(list_to_binary(PrivDir),<<"_build">>),
    tts_file_util:to_abs(Dir, binary_to_list(BinBaseDir)).

wait_for_oidcc_ready() ->
    {ok, OidcList} = oidcc:get_openid_provider_list(),
    wait_for_oidcc_ready(OidcList).
wait_for_oidcc_ready(List) ->
    Ready = fun({Id, Pid}, NotReady) ->
                    case oidcc_openid_provider:is_ready(Pid) of
                        true -> NotReady;
                        false -> [{Id, Pid} | NotReady]
                    end
            end,
    case lists:foldl(Ready, [], List) of
        [] -> ok;
        NewList ->
            timer:sleep(200),
            wait_for_oidcc_ready(NewList)
    end.



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
