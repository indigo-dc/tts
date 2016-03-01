-module(tts_services).

-export([get_service_list/0]).
-export([get_service_list/1]).
-export([get_service_info/1]).
-export([add_service/2]).


get_service_list() ->
    {ok,[]}.

get_service_list(_UserId) ->
    %TODO: implement a whitelist per service
    tts_data:service_get_list().

get_service_info(ServiceId) ->
    tts_data:service_get(ServiceId).

add_service(ServiceId,ServiceInfo) ->
    tts_data:service_add(ServiceId,ServiceInfo).

