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

-export([disable/1]).
-export([enable/1]).
-export([is_enabled/1]).
-export([allows_same_state/1]).
-export([get_credential_limit/1]).

get_list() ->
     tts_data:service_get_list().

get_list(#{uid := UserId})  ->
    get_list(UserId);
get_list(UserId) ->
    %TODO: implement a whitelist per service
    {ok, ServiceList} = tts_data:service_get_list(),
    UpdateLimit = fun(Service, List) ->
                      #{ id := ServiceId
                       } = Service,
                      Limit = maps:get(cred_limit, Service, 0),
                      {ok, Count} = tts_credential:get_count(UserId, ServiceId),
                      LimitReached = (Count >= Limit),
                      Update = #{ limit_reached => LimitReached,
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
    tts_data:service_add(ServiceId, maps:put(enabled, true, ServiceInfo));
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
                    {<<"CredentialLimit">>, cred_limit},

                    {<<"ConnectionType">>, con_type},
                    {<<"ConnectionUser">>, con_user},
                    {<<"ConnectionPassword">>, con_pass},
                    {<<"ConnectionHost">>, con_host},
                    {<<"ConnectionPort">>, con_port},
                    {<<"ConnectionSshDir">>, con_ssh_user_dir},
                    {<<"ConnectionSshAutoAcceptHosts">>, con_ssh_auto_accept},

                    {<<"AllowSameState">>, allow_same_state},

                    {<<"Cmd">>, cmd},


                    {<<"ssh">>, ssh},
                    {<<"local">>, local},
                    {<<"none">>, local},

                    {<<"undefined">>, undefined},
                    {<<"true">>, true}
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
verify_value(con_ssh_auto_accept, Value) ->
    {ok, bin_to_atom(Value, false)};
verify_value(cred_limit, Limit) ->
    {ok, list_to_integer(Limit)};
verify_value(AKey, Value) when is_list(Value) ->
    % default is to convert to binary
    verify_value(AKey, list_to_binary(Value));
verify_value(con_type, Value) ->
    {ok, bin_to_atom(Value, undefined)};
verify_value(allow_same_state, Value) ->
    {ok, bin_to_atom(Value, false)};
verify_value(_AKey, Value) ->
    {ok, Value}.

