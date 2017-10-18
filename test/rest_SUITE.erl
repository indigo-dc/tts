-module(rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("watts.hrl").

-export([all/0,
         groups/0,
         %% group/1,
         %% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         %% init_per_testcase/2,
         %% end_per_testcase/2,

         python2_check/1,
         service_config/1,
         provider_config/1,
         wattson_version/1,
         init_done/1,
         %% provider_list/0,
         rest_communication_v1/1,
         rest_communication_v2/1
        ]).

all() ->
    [
     {group, sqlite},
     {group, mnesia},
     {group, eleveldb}
     %% init_done,
     %% python2_check,
     %% provider_config,
     %% wattson_version,
     %% service_config,
     %% rest_communication_v2,
     %% rest_communication_v1
    ].

groups() ->
    [{sqlite, [sequence],[
     init_done,
     python2_check,
     provider_config,
     wattson_version,
     service_config,
     rest_communication_v2,
     rest_communication_v1]},
    {mnesia, [sequence],[
     init_done,
     python2_check,
     provider_config,
     wattson_version,
     service_config,
     rest_communication_v2,
     rest_communication_v1]},
    {eleveldb, [sequence],[
     init_done,
     python2_check,
     provider_config,
     wattson_version,
     service_config,
     rest_communication_v2,
     rest_communication_v1]}
     ].

%%
%% group(_) ->
%%     [].
%%
%% suite() ->
%%     [].

init_per_suite(Conf) ->
    Conf.




end_per_suite(Conf) ->
    Conf.

init_per_group(Group, Conf) ->
    setup_group(Group),
    Conf.

end_per_group(_Group, Conf) ->
    ok = application:stop(watts),
    ok = application:stop(jobs),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
    ok = application:stop(oidcc),
    ok = application:stop(oidcc_cowboy),
    Conf.

%% init_per_testcase(_TestCase, Conf) ->
%%      mock_oidcc(),
%%      Conf.
%%
%% end_per_testcase(_TestCase, Conf) ->
%%     unmock_oidcc(),
%%     Conf.


setup_group(DbType) ->
    config_db(DbType),
    ServiceList = [#{allow_same_state => false,
                  authz => #{allow => [{any,<<"sub">>,any,true}],
                    forbid => [],
                    hide => false,
                    tooltip => <<"Tooltip">>},
                  cmd => <<"/tmp/watts_common_test/tts_plugin_info/plugin/info.py">>,
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
                   description => <<"Google">>,
                   config_endpoint=> <<"https://accounts.google.com/.well-known/openid-configuration">>,
                   id => <<"google">>,
                   disable_login => false,
                   scopes => [<<"openid">>,<<"profile">>]}],
    ?SETCONFIG(start_time, erlang:system_time(seconds)),
    ?SETCONFIG(secret_dir, secret_dir()),
    ?SETCONFIG(max_provider_wait, 1),
    ?SETCONFIG(service_list, ServiceList),
    ?SETCONFIG(provider_list, ProviderList),
    ?SETCONFIG(session_timeout, 90000),
    ?SETCONFIG(allow_dropping_credentials, true),
    ?SETCONFIG(redirection_enable, false),
    ?SETCONFIG(redirection_port, 8080),
    ?SETCONFIG(ssl, false),
    ?SETCONFIG(port, 8080),
    ?SETCONFIG(enable_ipv6, false),
    ?SETCONFIG(num_acceptors, 100),
    ?SETCONFIG(listen_port, 8080),
    ?SETCONFIG(ep_main, <<"/">>),
    ?SETCONFIG(hostname, "localhost"),
    ?UNSETCONFIG(watts_init_started),
    application:set_env(oidcc, cert_depth, 5),
    application:set_env(oidcc, cacertfile, ca_file()),
    {ok, _} = application:ensure_all_started(watts),
    ok.


config_db(sqlite) ->
    ?SETCONFIG(database_type, sqlite),
    ?SETCONFIG(persistent_module, watts_persistent_sqlite),
    ?SETCONFIG(sqlite_db, "/tmp/watts_common_test/watts_ct.db");
config_db(mnesia) ->
    ?SETCONFIG(database_type, mnesia),
    ?SETCONFIG(persistent_module, watts_persistent_mnesia),
    ?SETCONFIG(mnesia_dir, "/tmp/watts_common_test/mnesia");
config_db(eleveldb) ->
    ?SETCONFIG(database_type, eleveldb),
    ?SETCONFIG(persistent_module, watts_persistent_leveldb),
    ?SETCONFIG(eleveldb_dir, "/tmp/watts_common_test/eleveldb").

service_config(_Config) ->
    {ok, Services} = watts_service:get_list(),
    ct:log("service configs:~n~p~n", [Services]),
    ok.

provider_config(_Config) ->
    WaitList = fun() ->
                   {ok, Provider} = watts:get_openid_provider_list(),
                   length(Provider) >= 1
           end,
    ok = test_util:wait_for_true(WaitList, 10),
    WaitReady = fun() ->
                   {ok, Provider} = watts:get_openid_provider_list(),
                   io:format("Provider: ~p~n",[Provider]),
                   [#{ready := Result}| _] = Provider,
                   Result
           end,
    ok = test_util:wait_for_true(WaitReady, 1000),
    {ok, Provider} = watts:get_openid_provider_list(),
    ct:log("provider configs:~n~p~n", [Provider]),
    ok.

init_done(_Config) ->
    Done = fun() ->
                   Children = supervisor:which_children(watts_sup),
                   case lists:keyfind(init, 1, Children) of
                       {init, undefined, worker, _} ->
                           true;
                       _ ->
                           false
                   end
           end,
    ok = test_util:wait_for_true(Done, 500),
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

wattson_version(_Config) ->
    perform_rest_request("--version", 2),
    ok.

rest_communication_v1(_Config) ->
    rest_communication(1).


rest_communication_v2(_Config) ->
    {ok, Info} = perform_rest_request("info", 2),
    true = is_valid_info(Info),
    rest_communication(2).

rest_communication(Version) ->
    mock_oidcc(),
    Issuer = <<"https://accounts.google.com">>,

    {ok, ProviderList} = perform_rest_request("lsprov", Version),
    {ok, _ProvId} = validate_provider_list(ProviderList, Issuer),
    {ok, ServiceList} = perform_rest_request("lsserv", Version),
    {ok, ServiceId} = validate_service_list(ServiceList),
    {ok, CredList} = perform_rest_request("lscred", Version),
    true = is_empty_credential_list(CredList),
    {ok, CredentialData} = perform_rest_request("request "++binary_to_list(ServiceId), Version),
    {ok, CredId} = validate_credential(CredentialData, Version),
    {ok, CredList2} = perform_rest_request("lscred", Version),
    false = is_empty_credential_list(CredList2),
    true = is_in_cred_list(CredId, CredList2, Version),
    {ok, _Result} = perform_rest_request("revoke "++binary_to_list(CredId), Version),
    {ok, CredList3} = perform_rest_request("lscred", Version),
    true = is_empty_credential_list(CredList3),
    unmock_oidcc(),
    ok.

is_valid_info(#{display_name := _, logged_in := false, redirect_path := _,
                version := _}) ->
    true;
is_valid_info(_) ->
    false.




validate_provider_list(#{openid_provider_list := List}, Issuer) ->
    ct:log("provider list entries: ~p",[List]),
    find_provider(List, Issuer).

find_provider([], _Issuer) ->
    {error, not_found};
find_provider([#{issuer := Issuer, id := Id} | _T], Issuer) ->
    {ok, Id};
find_provider([_H | T], Issuer) ->
    find_provider(T, Issuer).

validate_service_list(#{service_list := List}) ->
    [#{id := Id} | _ ] = List,
    {ok, Id}.

is_empty_credential_list(#{credential_list := List}) ->
    List == [].


validate_credential(#{credential := #{entries := List}}, 1)  ->
    Filter = fun(#{name := Name}) ->
                     Name == <<"id">>
             end,
    case lists:filter(Filter, List) of
        [#{value := Id}] -> {ok, Id};
        _ -> {error, not_found}
    end;
validate_credential(#{credential := #{id := Id}}, 2)  ->
    {ok, Id};
validate_credential(_, 2)  ->
        {error, not_found}.

is_in_cred_list(CredId, #{credential_list := List}, Version) ->
    Findv1 = fun(#{id := #{cred_id := CId}}, Result) ->
                   case CId == CredId of
                       true -> true;
                       _ -> Result
                   end
           end,
    Findv2 = fun(#{cred_id:= CId}, Result) ->
                   case CId == CredId of
                       true -> true;
                       _ -> Result
                   end
             end,
    Find = case Version of
               1 -> Findv1;
               2 -> Findv2
           end,
    lists:foldl(Find, false, List).








%%%%%% internal
perform_rest_request(Params, ProtVer) ->
    ExtraParams = case ProtVer of
                      2 -> " --json -p 2 ";
                      1 -> " --json -p 1 "
                  end,
    Exec = "/tmp/watts_common_test/wattson/wattson",
    Issuer = case ProtVer of
                 1 -> "https://accounts.google.com";
                 _ -> "google"
             end,
    Cmd = "export WATTSON_TOKEN=MockToken && export WATTSON_ISSUER="++Issuer++" && export WATTSON_URL=http://localhost:8080 && "++Exec++ExtraParams++Params,
    Result = os:cmd(Cmd),
    ct:log("executed '~s ~s ~s' with result ~p~n",[Exec, ExtraParams, Params, Result]),
    case jsone:try_decode(list_to_binary(Result), [{keys, attempt_atom}, {object_format, map}]) of
        {ok, Map, _} ->
            ct:log("  parsed to: ~p~n",[Map]),
            {ok, Map};
        {error, Reason} ->
            ct:log("  parsing error: ~p~n",[Reason]),
            {error, bad_json_result}
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

ca_file() ->
    code:where_is_file("cacert.pem").

secret_dir() ->
    filename:dirname(code:where_is_file("jwt.key")).
