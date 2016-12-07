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

-export([exists/1]).
-export([is_enabled/1]).
-export([is_allowed/2]).
-export([allows_same_state/1]).
-export([get_credential_limit/1]).
-export([get_queue/1]).

get_list() ->
     tts_data:service_get_list().

get_list(UserInfo) ->
    {ok, ServiceList} = get_list(),
    UpdateLimit = fun(Service, List) ->
                      #{ id := ServiceId,
                         authz := AuthzConf
                       } = Service,
                      Limit = maps:get(cred_limit, Service, 0),
                      {ok, Count} = tts_plugin:get_count(UserInfo,
                                                             ServiceId),
                      LimitReached = (Count >= Limit),
                      Update = #{ limit_reached => LimitReached,
                                  cred_limit => Limit,
                                  cred_count => Count
                                },
                          case is_allowed(ServiceId, UserInfo, AuthzConf) of
                              true ->
                                  [ maps:merge(Service, Update) | List];
                              _ ->
                                  List
                          end
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


exists(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, _} ->
            true;
        _ ->
            false
     end.


get_queue(ServiceId) ->
    case tts_data:service_get(ServiceId) of
        {ok, {ServiceId, Info}} ->
            QueueId = gen_queue_name(ServiceId),
            case maps:get(parallel_runner, Info) of
                infinite ->
                    {ok, undefined};
                Num when is_number(Num) ->
                    {ok, QueueId}
            end;
        _ -> {ok, undefined}
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

is_allowed(UserInfo, ServiceId) ->
    case get_info(ServiceId) of
        {ok, #{authz := AuthzConf} } ->
            is_allowed(ServiceId, UserInfo, AuthzConf);
        _ ->
            false
    end.

is_allowed(ServiceId, UserInfo, AuthzConf) ->
    tts_service_authz:is_authorized(ServiceId, UserInfo, AuthzConf).


add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    AuthzConf0 = maps:get(authz, ServiceInfo, #{allow => [], forbid => []}),
    {ok, AuthzConf} = tts_service_authz:validate_config(ServiceId, AuthzConf0),
    Update = #{enabled => false, authz => AuthzConf},
    tts_data:service_add(ServiceId, maps:merge(ServiceInfo, Update)),
    {ok, ServiceId};
add(_ServiceMap)  ->
    {error, invalid_config}.


update_params(Id) ->
    Service = tts_data:service_get(Id),
    get_and_validate_parameter(Service).

get_and_validate_parameter({ok, {Id, Info}}) ->
    Result = tts_plugin:get_params(Id),
    validate_params_and_update_db(Id, Info, Result);
get_and_validate_parameter(_) ->
    {error, not_found}.


validate_params_and_update_db(Id, Info, {ok, ParamMap}) ->
    NeededKeys = [conf_params, request_params, version],
    MissingKeys = NeededKeys -- maps:keys(ParamMap),
    case MissingKeys of
        [] ->
            #{conf_params := ConfParams,
              request_params := RequestParams,
              version := Version} = ParamMap,
            Ensure = #{plugin_conf => #{},
                       params => [],
                       plugin_version => Version
                      },
            Info0 = maps:merge(Info, Ensure),
            lager:info("service ~p: plugin version ~p", [Id, Version]),
            {ValidConfParam, Info1}=validate_conf_parameter(ConfParams, Info0),
            {ValidCallParam, Info2}=validate_call_parameter_sets(RequestParams,
                                                                 Info1),
            Info3 = list_skipped_parameter_and_delete_config(Info2),
            IsValid = ValidConfParam and ValidCallParam,
            Update = #{enabled => IsValid},
            NewInfo = maps:merge(Info3, Update),
            start_runner_queue_if_needed(NewInfo),
            update_service(Id, NewInfo);
        _ ->
            lager:error("service ~p: missing keys in parameter response: ~p",
                        [Id, MissingKeys]),
            {error, bad_config}
    end;
validate_params_and_update_db(Id, _, {error, Result}) ->
    case maps:get(log_msg, Result, undefined) of
        undefined ->
            lager:error("service ~p: bad parameter response (from plugin)",
                        [Id]);
        Msg ->
            lager:error("service ~p: ~s", [Id, Msg])
    end,
    {error, bad_config}.

validate_conf_parameter(Params, Info) ->
    validate_conf_parameter(Params, Info, true).
validate_conf_parameter([], Info, Result) ->
    {Result, Info};
validate_conf_parameter([ Entry | T ], #{ id:= Id, cmd:= Cmd} =Info, Current) ->
    RequiredKeys = [name, default, type],
    MissingKeys =  RequiredKeys -- maps:keys(Entry),
    case MissingKeys of
        [] ->
            #{name := Name, default := Def, type := Type} = Entry,
            AtomType = to_conf_type(Type),
            Default = convert_to_type(Def, AtomType),
            {Res, NewInfo} = update_conf_parameter(Name, Default,
                                                   AtomType, Info),
            validate_conf_parameter(T, NewInfo, Res and Current);
        _ ->
            lager:error("service ~p: conf parameter missing keys ~p [~p]",
                        [Id, MissingKeys, Cmd]),
            {false, Info}
    end;
validate_conf_parameter(_, #{id := Id, cmd := Cmd} = Info, _) ->
    lager:error("service ~p: bad conf parameter for plugin ~p", [Id, Cmd]),
    {false, Info}.


update_conf_parameter(Name, _Default, unknown, #{id := Id} = Info) ->
    Msg = "service ~p: unsupported datatype at conf parameter ~p (from plugin)",
    lager:error(Msg, [Id, Name]),
    {false, Info};
update_conf_parameter(Name, {ok, Default}, Type,
                      #{id := Id, plugin_conf_config:=RawConf,
                        plugin_conf:= Conf} = Info) ->
    EMsg = "service ~p: bad configuration ~p: ~p, using default: ~p",
    WMsg = "service ~p: plugin config ~p not set, using default: ~p",
    Value =
        case maps:is_key(Name, RawConf) of
            true ->
                Val = maps:get(Name, RawConf),
                case convert_to_type(Val, Type) of
                    {ok, V} -> V;
                    _ ->
                        lager:warning(EMsg, [Id, Name, Val, Default]),
                        Default
                end;
            false ->
                lager:warning(WMsg, [Id, Name, Default]),
                Default
        end,
    NewConf = maps:put(Name, Value, Conf),
    {true, maps:put(plugin_conf, NewConf, Info)};
update_conf_parameter(Name, _, _Type, #{id := Id} = Info) ->
    lager:error("service ~p: bad default at conf parameter ~p (from plugin)",
                [Id, Name]),
    {false, Info}.

validate_call_parameter_sets(Params, Info) ->
    validate_call_parameter_sets(Params, Info, true).

validate_call_parameter_sets([], #{params := Params} =Info, Result) ->
    ValidInfo = case Params of
                    [] -> maps:put(params, [[]], Info);
                    _ -> maps:put(params, lists:reverse(Params), Info)
                end,
    {Result, ValidInfo};
validate_call_parameter_sets([ H | T ], Info, Current)
  when is_list(H) ->
    {Result, NewInfo} = validate_call_parameter_set(H, Info),
   validate_call_parameter_sets(T, NewInfo, Result and Current);
validate_call_parameter_sets([ H | T ], #{id := Id} = Info, _) ->
    lager:error("service ~p: bad request parameter set ~p (from plugin)",
                [Id, H]),
    validate_call_parameter_sets(T, Info, false).


validate_call_parameter_set(Set, Info) ->
    validate_call_parameter_set(Set, Info, [], true).
validate_call_parameter_set([], #{params := Params} = Info, ParamSet, Result)->
    NewParams = [ParamSet | Params ],
    NewInfo = maps:put(params, NewParams, Info),
    {Result, NewInfo};
validate_call_parameter_set([Param | T], #{id := Id} = Info, ParamSet, Current)
  when is_map(Param) ->
    RequiredKeys = [description, name, key, type],
    MissingKeys = RequiredKeys -- maps:keys(Param),
    case MissingKeys of
        [] ->
            {Result, NewParamSet}=validate_call_parameter(Param, Id, ParamSet),
            validate_call_parameter_set(T, Info, NewParamSet,
                                        Current and Result);
        _ ->
            EMsg = "service ~p: request parameter ~p is missing keys ~p",
            lager:error(EMsg, [Id, Param, MissingKeys]),
            validate_call_parameter_set(T, Info, ParamSet, false)
    end;
validate_call_parameter_set([H | T], #{id := Id} = Info, ParamSet, _Current) ->
    EMsg = "service ~p: bad request parameter ~p (from plugin)",
    lager:error(EMsg, [Id, H]),
    validate_call_parameter_set(T, Info, ParamSet, false).


validate_call_parameter(#{ key := Key, name := Name, description := Desc,
                           type:= Type} = Param, Id, ParamSet)
  when is_binary(Key), is_binary(Name), is_binary(Desc) ->
    EMsg = "service ~p: parameter ~p: bad type ~p (from plugin)",
    WMsg = "service ~p: parameter ~p:bad mandatory value ~p, using false",
    Mdtory = maps:get(mandatory, Param, false),
    Mandatory =
        case convert_to_type(Mdtory, boolean) of
            {error, _} ->
                lager:warning(WMsg, [Id, Name, Mdtory]),
                false;
            {ok, Bool} -> Bool
        end,

    {Result, NewParamSet } =
        case to_request_type(Type) of
            unknown ->
                lager:error(EMsg, [Id, Name, Type]),
                {false, ParamSet};
            AtomType ->
                {true, [#{ key => Key,
                           name => Name,
                           description => Desc,
                           type => AtomType,
                           mandatory => Mandatory
                         } | ParamSet]}
        end,
    {Result, NewParamSet };
validate_call_parameter(Param, Id, ParamSet) ->
    EMsg = "service ~p: bad request parameter values ~p (not strings)",
    lager:error(EMsg, [Id, Param]),
    {false, ParamSet}.




list_skipped_parameter_and_delete_config(#{plugin_conf := Conf,
                                           plugin_conf_config := RawConf,
                                           id := Id
                                          }
                                         = Info) ->
    WMsg = "service ~p: skipping unknown parameter ~p = ~p from configuration",
    Keys = maps:keys(RawConf),
    Warn =
        fun(Key, _) ->
                case maps:is_key(Key, Conf) of
                    false ->
                        lager:warning(WMsg, [Id, Key, maps:get(Key, RawConf)]);
                    _ ->
                        ok
                end
        end,
    lists:foldl(Warn, ok, Keys),
    maps:remove(plugin_conf_config, Info).


start_runner_queue_if_needed(#{enabled := true,
                               parallel_runner := NumRunner,
                               id := Id
                              })
  when is_number(NumRunner) ->
    QueueId = gen_queue_name(Id),
    ok = jobs:add_queue(QueueId, [{regulators, [
                                           {counter, [{limit, NumRunner}]}
                                          ]},
                             {type, fifo}
                            ]),
    Msg = "service ~p: queue ~p started with max ~p parallel runners",
    lager:info(Msg, [Id, QueueId, NumRunner]);
start_runner_queue_if_needed(_) ->
    ok.



update_service(Id, NewInfo) when is_map(NewInfo) ->
    tts_data:service_update(Id, NewInfo);
update_service( _, _) ->
    {error, invalid_config}.

convert_to_type(Value, string)
  when is_binary(Value) ->
    {ok, Value};
convert_to_type(true, boolean) ->
    {ok, true};
convert_to_type(<<"True">>, boolean) ->
    {ok, true};
convert_to_type(<<"true">>, boolean) ->
    {ok, true};
convert_to_type(false, boolean) ->
    {ok, false};
convert_to_type(<<"False">>, boolean) ->
    {ok, false};
convert_to_type(<<"false">>, boolean) ->
    {ok, false};
convert_to_type(_, _ ) ->
    {error, bad_value}.



to_atom(Type)
  when is_binary(Type) ->
    try
        binary_to_existing_atom(Type, utf8)
    catch error:badarg ->
            unknown
    end;
to_atom(Type)
  when is_list(Type) ->
    to_atom(list_to_binary(Type));
to_atom(_) ->
    unknown.

to_conf_type(Type) ->
    ValidTypes = [boolean, string],
    to_valid_type(Type, ValidTypes).

to_request_type(Type) ->
    ValidTypes = [textarea],
    to_valid_type(Type, ValidTypes).

to_valid_type(Type, ValidTypes) ->
    AType = to_atom(Type),
    case lists:member(AType, ValidTypes) of
        true ->
            AType;
        _ ->
            unknown
    end.

gen_queue_name(Id) when is_binary(Id) ->
    Dash = <<"-">>,
    Module = atom_to_binary(?MODULE, utf8),
    QueueName = << Module/binary, Dash/binary, Id/binary >>,
    binary_to_atom(QueueName, utf8).
