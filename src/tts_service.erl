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

add(ServiceId,ServiceInfo) when is_map(ServiceInfo), is_binary(ServiceId) ->
    %TODO:
    %validate and change keys/values
    tts_data:service_add(ServiceId,maps:put(id,ServiceId,ServiceInfo));
add(ServiceId,ServiceInfo) when is_list(ServiceId) ->
    add(list_to_binary(ServiceId), ServiceInfo).

