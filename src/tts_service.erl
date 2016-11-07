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

-export([is_enabled/1]).
-export([is_allowed/2]).
-export([allows_same_state/1]).
-export([get_credential_limit/1]).
%% -export([group_plugin_configs/1]).

get_list() ->
     tts_data:service_get_list().

get_list(UserInfo) ->
    %TODO: implement a whitelist/blacklist per service
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

is_allowed(_UserInfo, _ServiceId) ->
    %% TODO: implement a white/blacklist
    true.


add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    tts_data:service_add(ServiceId, maps:put(enabled, false, ServiceInfo)),
    {ok, ServiceId};
add(_ServiceMap)  ->
    {error, invalid_config}.


update_params(Id) ->
    Service = tts_data:service_get(Id),
    get_and_validate_parameter(Service).

get_and_validate_parameter({ok, {Id, Info}}) ->
    Result = tts_credential:get_params(Id),
    validate_params_and_update_db(Id, Info, Result);
get_and_validate_parameter(_) ->
    {error, not_found}.


validate_params_and_update_db(Id, Info, {ok, ConfParams, RequestParams}) ->
    Update = #{enabled => true, params => RequestParams,
               conf_params => ConfParams},
    NewInfo = maps:merge(Info, Update),
    IsValid = validate_parameter(RequestParams, ConfParams),
    update_service(IsValid, Id, NewInfo);
validate_params_and_update_db(_, _, _) ->
    {error, bad_config}.

validate_parameter(_, _) ->
    %% TODO: implement a logic for config validation
    true.

update_service(true, Id, NewInfo) when is_map(NewInfo) ->
    tts_data:service_update(Id, NewInfo);
update_service(_, _, _) ->
    {error, update}.
