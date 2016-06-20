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
         init_per_testcase/2,
         end_per_testcase/2,

         update_config/1,
         service_config/1,
         %% provider_list/0,
         provider_list/1,
         service_list/1,
         credential_list/1,
         credential_request/1,
         credential_revoke/1
        ]).

all() ->
    [
     update_config,
     service_config,
     provider_list,
     service_list,
     credential_request,
     credential_list,
     credential_revoke
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
init_per_testcase(_TestCase, Conf) ->
     mock_oidcc(),
     Conf.

end_per_testcase(_TestCase, Conf) ->
    unmock_oidcc(),
    Conf.


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

provider_list(_Config) ->
    {ok, _Result} = perform_rest_request("lsprov localhost:8080"),
    ok.

service_list(_Config) ->
    {ok, _Result} = perform_rest_request("lsserv localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    ok.


credential_list(_Config) ->
    {ok, _Result} = perform_rest_request("lscred localhost:8080 MockToken https://iam-test.indigo-datacloud.eu/"),
    ok.

credential_request(_Config) ->
    ServiceId = "1",
    {ok, _Result} = perform_rest_request("request localhost:8080 ssh MockToken https://iam-test.indigo-datacloud.eu/"),
    ok.

credential_revoke(_Config) ->
    Issuer = <<"https://iam-test.indigo-datacloud.eu/">>,
    Subject = <<"tts_test_user">>,
    {ok, #{ site := #{uid := UserId}}} = tts_user_cache:get_user_info(Issuer, Subject),
    {ok, [#{cred_id := CredIdBin} | _]} = tts_credential:get_list(UserId),
    CredId = binary_to_list(CredIdBin),
    {ok, _Result} = perform_rest_request("revoke localhost:8080 "++CredId++" MockToken https://iam-test.indigo-datacloud.eu/"),
    ok.

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
