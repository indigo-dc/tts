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

         update_config/1,
         %% provider_list/0,
         provider_list/1


        ]).

all() ->
    [
     update_config,
     provider_list
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
    mock_oidcc(),
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
%%     Conf.
%%
%% end_per_testcase(_TestCase, Conf) ->
%%     Conf.


update_config(_Config) ->
    ConfigDir = tts_based_dir("./test/config_ct"),
    ct:pal("config-dir: ~p",[ConfigDir]),
    tts_config:reload(ConfigDir),
    wait_for_config(),
    AppConf = application:get_all_env(tts),
    ct:pal("the configuration is: ~p",[AppConf]),
    ok.

provider_list(_Config) ->
    {ok, _Json} = perform_rest_request("lsprov localhost"),
    ok.


perform_rest_request(Params) ->
    Script = tts_based_dir("./client/ttsc.sh"),
    Cmd = "URI_SCHEME=http && "++Script++" "++Params,
    Json = os:cmd(Cmd),
    ct:log("executed ~p with result ~p",[Cmd, Json]),
    case jsx:is_json(Json) of
        true -> {ok, jsx:decode(Json, [return_maps, {labels, attempt_atom}])};
        false -> {error, not_json, Json}
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

mock_oidcc() ->
    ok.


