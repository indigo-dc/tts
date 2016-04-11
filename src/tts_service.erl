-module(tts_service).

-export([get_list/0]).
-export([get_list/1]).
-export([get_info/1]).
-export([add/1]).


get_list() ->
    {ok, []}.

get_list(UserId) ->
    %TODO: implement a whitelist per service
    {ok, ServiceList} = tts_data:service_get_list(),
    {ok, CredList} = tts_credential:get_list(UserId),
    UpdateService = fun(Map, List) ->
                            ServiceId = maps:get(id, Map, undefined),
                            HasCred = lists:member(ServiceId, CredList),
                            [maps:put(has_credential, HasCred, Map) | List]
                    end,
    Result = lists:reverse(lists:foldl(UpdateService, [], ServiceList)),
    {ok, Result}.

get_info(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, Info};
        Other -> Other
    end.

add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    tts_data:service_add(ServiceId, ServiceInfo);
add(#{ id := ServiceId } = ServiceMap) when is_list(ServiceId) ->
    add(maps:put(id, list_to_binary(ServiceId), ServiceMap));
add(ServiceMap) when is_map(ServiceMap) ->
    ServiceInfo = map_to_atom_keys(ServiceMap),
    add(ServiceInfo);
add(_ServiceMap)  ->
    {error, invalid_config}.


-include("tts.hrl").
-include_lib("public_key/include/public_key.hrl").

map_to_atom_keys(ServiceMap) ->
    List = maps:to_list(ServiceMap),
    map_to_atom_keys(List, #{}).

map_to_atom_keys([], Map) ->
    Map;
map_to_atom_keys([{Key, Value}|T], Map) when is_binary(Key) ->
    AKey = bin_to_atom(Key),
    NewMap = case verify_value(AKey, Value) of
                 {ok, VerifiedValue} ->
                     maps:put(AKey, VerifiedValue, Map);
                 {ok, VerifiedValue, OtherMap} ->
                     Map2 = maps:merge(Map, OtherMap),
                     maps:put(AKey, VerifiedValue, Map2)
             end,
    map_to_atom_keys(T, NewMap);
map_to_atom_keys([{Key, Value}|T], Map) when is_list(Key) ->
    map_to_atom_keys([{list_to_binary(Key), Value} | T], Map).

-define(KEYMAPPING, [
                    {<<"Id">>, id},
                    {<<"Type">>, type},
                    {<<"Host">>, host},
                    {<<"Port">>, port},
                    {<<"Description">>, description},

                    {<<"ConnectionType">>, con_type},
                    {<<"ConnectionUser">>, con_user},
                    {<<"ConnectionHost">>, con_host},
                    {<<"ConnectionPort">>, con_port},
                    {<<"ConnectionSshDir">>, con_ssh_user_dir},

                    {<<"RequestCmdFile">>, cred_cmd_req_file},
                    {<<"RevokeCmdFile">>, cred_cmd_rev_file},
                    {<<"IncidentCmdFile">>, cred_cmd_si_file},

                    {<<"ssh">>, ssh},
                    {<<"local">>, local},
                    {<<"none">>, local},

                    {<<"undefined">>, undefined}
                   ]).

bin_to_atom(BinaryKey) ->
    bin_to_atom(BinaryKey, BinaryKey).

bin_to_atom(BinaryKey, Default) ->
    case lists:keyfind(BinaryKey, 1, ?KEYMAPPING) of
        false ->
            Default;
        {_, AtomKey} ->
            AtomKey
    end.

verify_value(con_ssh_user_dir, SshDir) ->
    AbsSshDir = tts_file_util:to_abs(SshDir, ?CONFIG(service_config_path)),
    case filelib:is_dir(AbsSshDir) of
        true ->
            {ok, AbsSshDir};
        false ->
            {ok, <<"~/.ssh">>}
    end;
verify_value(con_user, User) ->
    {ok, User};
verify_value(con_host, Host) ->
    {ok, Host};
verify_value(con_port, Port) ->
    {ok, list_to_integer(Port)};
verify_value(cred_cmd_req_file, File) ->
    {ok, ModuleName} = load_command_file(File),
    {ok, File, #{cmd_mod_req => ModuleName}};
verify_value(cred_cmd_rev_file, File) ->
    {ok, ModuleName} = load_command_file(File),
    {ok, File, #{cmd_mod_rev => ModuleName}};
verify_value(cred_cmd_si_file, File) ->
    {ok, ModuleName} = load_command_file(File),
    {ok, File, #{cmd_mod_si => ModuleName}};
verify_value(AKey, Value) when is_list(Value) ->
    % default is to convert to binary
    verify_value(AKey, list_to_binary(Value));
verify_value(con_type, Value) ->
    {ok, bin_to_atom(Value, undefined)};
verify_value(_AKey, Value) ->
    {ok, Value}.

load_command_file(InFile) ->
    %load file
    BaseDir = ?CONFIG(service_config_path),
    % compile file to a module, store module name in config
    FileName = tts_file_util:to_abs(InFile, BaseDir),
    ModuleName = get_unique_module_name(),
    {ok, _} = erlydtl:compile_file(FileName, ModuleName),
    {ok, ModuleName}.

get_unique_module_name() ->
    Random = tts_utils:random_string(8),
    Prefix = <<"tts_service_cmd_">>,
    ModuleName = binary_to_atom(<< Prefix/binary, Random/binary >>, utf8),
    ensure_module_name_unused(code:is_loaded(ModuleName), ModuleName).

ensure_module_name_unused(false, ModuleName) ->
    ModuleName;
ensure_module_name_unused(_, _) ->
    get_unique_module_name().

