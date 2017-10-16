-module(watts_service).
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
-export([are_params_valid/2]).
-export([allows_same_state/1]).
-export([get_credential_limit/1]).
-export([get_queue/1]).

-spec get_list() -> {ok, [map()]}.
get_list() ->
     watts_data:service_get_list().

-spec get_list(UserInfo :: watts_userinfo:userinfo()) ->
                      {ok, [map()]}.
get_list(UserInfo) ->
    {ok, ServiceList} = get_list(),
    UpdateLimit
        = fun(Service, List) ->
                  #{ id := ServiceId,
                     authz := #{hide := Hide,
                                tooltip := Tooltip
                               } = AuthzConf
                   } = Service,
                  Limit = maps:get(cred_limit, Service, 0),
                  {ok, Count} = watts_plugin:get_count(UserInfo,
                                                     ServiceId),
                  LimitReached = (Count >= Limit),
                  Authz = is_allowed(ServiceId, UserInfo, AuthzConf),

                  Update = #{ limit_reached => LimitReached,
                              cred_limit => Limit,
                              cred_count => Count,
                              authorized => Authz,
                              authz_tooltip => Tooltip
                            },
                  Show = Authz or (not Hide),
                  case Show of
                      true ->
                          Keys = [id, description, type, host, port, enabled,
                                  cred_count, cred_limit, limit_reached, params,
                                  authorized, authz_tooltip, pass_access_token],
                          Entry = maps:with(Keys, maps:merge(Service, Update)),
                          [ Entry | List];
                      _ ->
                          List
                  end
          end,
    {ok, lists:reverse(lists:foldl(UpdateLimit, [], ServiceList))}.


get_info(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, Info};
        Other -> Other
    end.

get_credential_limit(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, maps:get(cred_limit, Info, 0)};
        _ -> {ok, 0}
    end.


exists(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, _} ->
            true;
        _ ->
            false
     end.

get_queue(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, maps:get(queue, Info, undefined)};
        _ -> {ok, undefined}
    end.

is_enabled(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(enabled, Info, false);
        _ -> false
    end.

allows_same_state(ServiceId) ->
    case watts_data:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(allow_same_state, Info, false);
        _ -> false
    end.

-spec is_allowed(UserInfo :: watts_userinfo:userinfo(), ServiceId :: binary())
                -> boolean().
is_allowed(UserInfo, ServiceId) ->
    case get_info(ServiceId) of
        {ok, #{authz := AuthzConf} } ->
            is_allowed(ServiceId, UserInfo, AuthzConf);
        _ ->
            false
    end.

-spec is_allowed( ServiceId :: binary(), UserInfo :: watts_userinfo:userinfo(),
                  AuthzConf :: watts_service_authz:config())
                -> boolean().
is_allowed(ServiceId, UserInfo, AuthzConf) ->
    watts_service_authz:is_authorized(ServiceId, UserInfo, AuthzConf).


are_params_valid(Params, #{params := ParamSets})
  when is_map(Params) ->
    ToBinary = fun(Key, List) when is_binary(Key) ->
                       [ Key | List ];
                 (Atom, List) when is_atom(Atom) ->
                       [ atom_to_binary(Atom, utf8) | List ]
               end,
    ParamList = lists:foldl(ToBinary, [], maps:keys(Params)),
    ExistsMatching = fun(ParamSet, Current) ->
                             Current or fulfills_paramset(ParamList, ParamSet)
                     end,
    lists:foldl(ExistsMatching, false, ParamSets);
are_params_valid(Params, ServiceId)
  when is_map(Params), is_binary(ServiceId) ->
    case get_info(ServiceId) of
        {ok, ServiceConfig} ->
            are_params_valid(Params, ServiceConfig);
        _ ->
            false
    end;
are_params_valid(_Params, _Service) ->
    false.


fulfills_paramset([], []) ->
    true;
fulfills_paramset(_, []) ->
    false;
fulfills_paramset([], List) ->
    OnlyNonMandatory = fun(#{mandatory := true}, _) ->
                               false;
                          (#{mandatory := false}, Cur) ->
                                  Cur
                          end,
    lists:foldl(OnlyNonMandatory, true, List);
fulfills_paramset(Params, [#{key := Key, mandatory := Mandatory} | ParamSet]) ->
    case { lists:member(Key, Params), Mandatory }  of
        {true, _} ->
            fulfills_paramset(lists:delete(Key, Params), ParamSet);
        {false, false} ->
            fulfills_paramset(lists:delete(Key, Params), ParamSet);
        {false, true} ->
            false
    end.





add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    AuthzConf0 = maps:get(authz, ServiceInfo, #{allow => [], forbid => []}),
    {ok, AuthzConf} = watts_service_authz:validate_config(ServiceId,
                                                          AuthzConf0),
    Update = #{enabled => false, authz => AuthzConf},
    ok = watts_data:service_add(ServiceId, maps:merge(ServiceInfo, Update)),
    {ok, ServiceId};
add(_ServiceMap)  ->
    {error, invalid_config}.


update_params(Id) ->
    Service = watts_data:service_get(Id),
    get_and_validate_parameter(Service).

get_and_validate_parameter({ok, {Id, Info}}) ->
    Result = watts_plugin:get_params(Id),
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
            ValidName = is_valid_key(Name),
            AtomType = to_conf_type(Type),
            Default = convert_to_type(Def, AtomType),
            {Res, NewInfo} = update_conf_parameter(Name, ValidName, Default,
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


update_conf_parameter(Name, _Valid, _Default, unknown, #{id := Id} = Info) ->
    Msg = "service ~p: unsupported datatype at conf parameter ~p (from plugin)",
    lager:error(Msg, [Id, Name]),
    {false, Info};
update_conf_parameter(Name, true, {ok, Default}, Type,
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
update_conf_parameter(Name, true, _, _Type, #{id := Id} = Info) ->
    lager:error("service ~p: bad default at conf parameter ~p (from plugin)",
                [Id, Name]),
    {false, Info};
update_conf_parameter(Name, false, _, _Type, #{id := Id} = Info) ->
    lager:error("service ~p: bad config parameter name '~p' (from plugin)",
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
    validate_call_parameter_set(Set, Info, [], [], true).
validate_call_parameter_set([], #{params := Params} = Info, ParamSet,
                            _Keys, Result)->
    NewParams = [ParamSet | Params ],
    NewInfo = maps:put(params, NewParams, Info),
    {Result, NewInfo};
validate_call_parameter_set([Param | T], #{id := Id} = Info, ParamSet, Keys,
                            Current) when is_map(Param) ->
    RequiredKeys = [description, name, key, type],
    MissingKeys = RequiredKeys -- maps:keys(Param),
    case MissingKeys of
        [] ->
            #{ key := Key,
               name := Name,
               description := Desc,
               type:= Type} = Param,
            ValidKey = is_valid_key(Key),
            KeyExists = lists:member(Key, Keys),
            {Result, NewParamSet}=validate_call_parameter(Key, ValidKey,
                                                          KeyExists, Name,
                                                          Desc, Type, Param, Id,
                                                          ParamSet),
            validate_call_parameter_set(T, Info, NewParamSet, Keys,
                                        Current and Result);
        _ ->
            EMsg = "service ~p: request parameter ~p is missing keys ~p",
            lager:error(EMsg, [Id, Param, MissingKeys]),
            validate_call_parameter_set(T, Info, ParamSet, Keys, false)
    end;
validate_call_parameter_set([H | T], #{id := Id} = Info, ParamSet,
                            Keys, _Current) ->
    EMsg = "service ~p: bad request parameter ~p (from plugin)",
    lager:error(EMsg, [Id, H]),
    validate_call_parameter_set(T, Info, ParamSet, Keys, false).


validate_call_parameter(Key, true, false, Name, Desc, Type, Param, Id,
                        ParamSet) when is_binary(Key), is_binary(Name),
                                       is_binary(Desc) ->
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
validate_call_parameter(Key, false, false,  _N, _D, _T, Param, Id, ParamSet) ->
    EMsg = "service ~p: key ~p of parameter contains spaces: ~p",
    lager:error(EMsg, [Id, Key, Param]),
    {false, ParamSet};
validate_call_parameter(Key, _, true,  _N, _D, _T, Param, Id, ParamSet) ->
    EMsg = "service ~p: key ~p exists multiple times: ~p",
    lager:error(EMsg, [Id, Key, Param]),
    {false, ParamSet};
validate_call_parameter(_,  true, false,  _, _, _, Param, Id, ParamSet) ->
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
                              } = Info)
  when is_number(NumRunner) ->
    QueueId = gen_queue_name(Id),
    ok = jobs:add_queue(QueueId, [{regulators, [
                                           {counter, [{limit, NumRunner}]}
                                          ]},
                             {type, fifo}
                            ]),
    Msg = "service ~p: queue ~p started with max ~p parallel runners",
    lager:info(Msg, [Id, QueueId, NumRunner]),
    {ok, maps:put(queue, QueueId, Info)};
start_runner_queue_if_needed(Info) ->
    {ok, Info}.




update_service(Id, NewInfo) when is_map(NewInfo) ->
    watts_data:service_update(Id, NewInfo);
update_service( _, _) ->
    {error, invalid_config}.

convert_to_type(Value, string)
  when is_binary(Value) ->
    {ok, Value};
convert_to_type(Value, boolean) ->
    TrueValues = [true, <<"True">>, <<"true">>],
    IsTrue = lists:member(Value, TrueValues),
    FalseValues = [false, <<"False">>, <<"false">>],
    IsFalse = lists:member(Value, FalseValues),
    case {IsTrue, IsFalse} of
        {true, false} ->
            {ok, true};
        {false, true} ->
            {ok, false};
        _ ->
            {error, bad_value}
    end;
convert_to_type(_, _) ->
    {error, bad_value}.



to_atom(Type) ->
    try
        binary_to_existing_atom(Type, utf8)
    catch error:badarg ->
            unknown
    end.

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

is_valid_key(Key) when is_binary(Key) ->
    CharList = binary_to_list(Key),
    IsValid = fun(Char, Current) ->
                      is_valid_key_char(Char) and Current
              end,
    lists:foldl(IsValid, true, CharList).


is_valid_key_char($a) ->
    true;
is_valid_key_char($b) ->
    true;
is_valid_key_char($c) ->
    true;
is_valid_key_char($d) ->
    true;
is_valid_key_char($e) ->
    true;
is_valid_key_char($f) ->
    true;
is_valid_key_char($g) ->
    true;
is_valid_key_char($h) ->
    true;
is_valid_key_char($i) ->
    true;
is_valid_key_char($j) ->
    true;
is_valid_key_char($k) ->
    true;
is_valid_key_char($l) ->
    true;
is_valid_key_char($m) ->
    true;
is_valid_key_char($n) ->
    true;
is_valid_key_char($o) ->
    true;
is_valid_key_char($p) ->
    true;
is_valid_key_char($q) ->
    true;
is_valid_key_char($r) ->
    true;
is_valid_key_char($s) ->
    true;
is_valid_key_char($t) ->
    true;
is_valid_key_char($u) ->
    true;
is_valid_key_char($v) ->
    true;
is_valid_key_char($w) ->
    true;
is_valid_key_char($x) ->
    true;
is_valid_key_char($y) ->
    true;
is_valid_key_char($z) ->
    true;
is_valid_key_char($0) ->
    true;
is_valid_key_char($1) ->
    true;
is_valid_key_char($2) ->
    true;
is_valid_key_char($3) ->
    true;
is_valid_key_char($4) ->
    true;
is_valid_key_char($5) ->
    true;
is_valid_key_char($6) ->
    true;
is_valid_key_char($7) ->
    true;
is_valid_key_char($8) ->
    true;
is_valid_key_char($9) ->
    true;
is_valid_key_char($_) ->
    true;
is_valid_key_char(_) ->
    false.
