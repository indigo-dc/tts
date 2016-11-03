-module(tts_service).
%%
%% Copyright 2016 SCC/KIT
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0 (see also the LICENSE file)
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-author("Bas Wegh, Bas.Wegh<at>kit.edu").


-export([get_list/0]).
-export([get_list/1]).
-export([get_info/1]).
-export([add/1]).
-export([update_params/1]).

-export([disable/1]).
-export([enable/1]).
-export([is_enabled/1]).
-export([allows_same_state/1]).
-export([get_credential_limit/1]).
%% -export([group_plugin_configs/1]).

get_list() ->
     tts_data:service_get_list().

get_list(UserInfo) ->
    %TODO: implement a whitelist per service
    {ok, ServiceList} = tts_data:service_get_list(),
    UpdateLimit = fun(Service, List) ->
                      #{ id := ServiceId
                       } = Service,
                      Limit = maps:get(cred_limit, Service, 0),
                      {ok, Count} = tts_credential:get_count(UserInfo,
                                                             ServiceId),
                      LimitReached = (Count >= Limit),
                      Update = #{ limit_reached => LimitReached,
                                  cred_limit => Limit,
                                  cred_count => Count
                                },
                      [ maps:merge(Service, Update) | List]
                  end,
    {ok, lists:reverse(lists:foldl(UpdateLimit, [], ServiceList))}.


get_info(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, Info};
        Other -> Other
    end.

get_credential_limit(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, maps:get(cred_limit, Info, 0)};
        _ -> {ok, 0}
    end.

is_enabled(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(enabled, Info, false);
        _ -> false
    end.

allows_same_state(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(allow_same_state, Info, false);
        _ -> false
    end.

enable(Id) ->
    set_enabled_to(true, Id).

disable(Id) ->
    set_enabled_to(false, Id).

set_enabled_to(Value, Id) ->
    case tts_data:service_get(Id) of
        {ok, {_Id, Info}} ->
            tts_data:service_update(Id, maps:put(enabled, Value, Info));
        Other -> Other
    end.


add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    tts_data:service_add(ServiceId, maps:put(enabled, false, ServiceInfo)),
    {ok, ServiceId};
add(_ServiceMap)  ->
    {error, invalid_config}.


update_params(Id) ->
    case tts_data:service_get(Id) of
        {ok, {Id, Info}} ->
            case tts_credential:get_params(Id) of
                {ok, ConfParams, RequestParams} ->
                    Update = #{enabled => true, params => RequestParams,
                               conf_params => ConfParams},
                    tts_data:service_update(Id, maps:merge(Info, Update));
                _ -> {error, update}
            end;
        _ -> {error, not_found}
    end.
