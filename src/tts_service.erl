-module(tts_service).

-export([get_list/0]).
-export([get_list/1]).
-export([get_info/1]).
-export([add/2]).


get_list() ->
    {ok,[]}.

get_list(_UserId) ->
    %TODO: implement a whitelist per service
    tts_data:service_get_list().

get_info(ServiceId) ->
    tts_data:service_get(ServiceId).

add(ServiceId,ServiceMap) when is_map(ServiceMap), is_binary(ServiceId) ->
    ServiceInfo = map_to_atom_keys(ServiceMap),
    tts_data:service_add(ServiceId,maps:put(id,ServiceId,ServiceInfo));
add(ServiceId,ServiceInfo) when is_list(ServiceId) ->
    add(list_to_binary(ServiceId), ServiceInfo).


-include("tts.hrl").

map_to_atom_keys(ServiceMap) ->
    List = maps:to_list(ServiceMap),
    map_to_atom_keys(List,#{}).

map_to_atom_keys([],Map) ->
    Map;
map_to_atom_keys([{Key,Value}|T],Map) when is_binary(Key) ->
    AKey = bin_to_atom(Key),
    NewMap = case verify_value(AKey,Value) of
                 {ok, VerifiedValue} ->  
                     maps:put(AKey, VerifiedValue, Map);
                 {ok, VerifiedValue, OtherMap} ->  
                     Map2 = maps:merge(Map, OtherMap), 
                     maps:put(AKey, VerifiedValue, Map2)
             end,
    map_to_atom_keys(T,NewMap);
map_to_atom_keys([{Key,Value}|T],Map) when is_list(Key) ->
    map_to_atom_keys([{list_to_binary(Key),Value} | T],Map).

-define(KEYMAPPING,[
                    {<<"Type">>,type},
                    {<<"Host">>,host},
                    {<<"Port">>,port},
                    {<<"Description">>,description},
                    
                    {<<"ConnectionType">>,con_type},
                    {<<"ConnectionUser">>,con_user},
                    {<<"ConnectionHost">>,con_host},
                    {<<"ConnectionPort">>,con_host},
                    {<<"ConnectionKeyFile">>,con_key_file},

                    {<<"RequestCmdFile">>,cred_cmd_req_file},
                    {<<"RevokeCmdFile">>,cred_cmd_rev_file},
                    {<<"IncidentCmdFile">>,cred_cmd_si_file},

                    {<<"ssh">>,ssh},
                    {<<"none">>,none},

                    {<<"undefined">>,undefined}
                   ]).

bin_to_atom(BinaryKey) ->
    bin_to_atom(BinaryKey, BinaryKey).

bin_to_atom(BinaryKey,Default) ->
    case lists:keyfind(BinaryKey,1,?KEYMAPPING) of
        false ->
            Default;
        {_, AtomKey} ->
            AtomKey
    end.

verify_value(AKey,Value) when is_list(Value) ->
    % default is to convert to binary
    verify_value(AKey,list_to_binary(Value));
verify_value(cred_cmd_req_file,InFile) ->
    %load file
    BaseDir = ?CONFIG(service_config_path),
    % compile file to a module, store module name in config
    FileName = tts_file_util:to_abs(InFile,BaseDir),
    RandomPart = tts_utils:random_string(8),
    ModuleName = binary_to_atom(<< <<"tts_service_cmd_">>/binary, RandomPart/binary >>, utf8), 
    {ok, _} = erlydtl:compile_file(FileName, ModuleName),
    {ok, InFile, #{cmd_mod_req => ModuleName}}; 
verify_value(con_type,Value) ->
    {ok, bin_to_atom(Value,undefined)}; 
verify_value(_AKey,Value) ->
    {ok, Value}.




