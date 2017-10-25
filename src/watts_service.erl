
%% @doc this module handles the services. It validates the parameter coming from
%% the plugin as as the configuration itself.
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


-export_type([info/0, limited_info/0, connection/0]).

-type info() :: #{ id => binary(),
                   description => binary(),
                   cmd => binary(),
                   cmd_env_use => boolean(),
                   cmd_env_var => string(),
                   cred_limit => infinite | integer(),
                   allow_same_state => boolean(),
                   plugin_conf => map(),
                   plugin_version => undefined | binary(),
                   params => [parameter_set()],
                   parallel_runner => infinite | integer(),
                   pass_access_token => boolean(),
                   plugin_timeout => infinity | integer(),
                   connection => connection(),
                   authz => watts_service_authz:config(),
                   queue => atom(),
                   enabled => boolean(),
                   _ => _
                 }.

-type parameter_set() :: [parameter()].
-type parameter() :: #{ key => binary(),
                            name => binary(),
                            description => binary(),
                            type => atom(),
                            mandatory => boolean() }.

-type conf_parameters() :: [conf_parameter()].
-type conf_parameter() :: #{ name => binary(),
                             type => binary(),
                             default => any() }.

-type limited_info() :: #{ id => binary(),
                           description => binary(),
                           cred_limit => infinite | integer(),
                           pass_access_token => boolean(),
                           enabled => boolean(),
                           params => [],

                           limit_reached => boolean(),
                           cred_count => integer(),
                           authorized => boolean(),
                           authz_tooltip => binary()
                         }.

-type connection() :: #{ type => local | ssh,
                         user => undefined | string(),
                         passwd => undefined | string(),
                         host => undefined | string(),
                         port => undefined | integer(),
                         ssh_dir => undefined | string(),
                         ssh_key_pass => undefined | string()
                       }.


%% @doc get a list of all services currently configured.
-spec get_list() -> {ok, [info()]}.
get_list() ->
     watts_ets:service_get_list().


%% @doc get the list of all services for a user
-spec get_list(UserInfo :: watts_userinfo:userinfo()) ->
                      {ok, [limited_info()]}.
get_list(UserInfo) ->
    {ok, ServiceList} = get_list(),
    {ok, ServicesOfUser} = filter_list_for_user(UserInfo, ServiceList),
    update_limits_for_user(UserInfo, ServicesOfUser).


%% @doc filter the list of services for a user, to hide them.
-spec filter_list_for_user(watts_userinfo:info(), [info()]) ->
                                  {ok, [info()]}.
filter_list_for_user(UserInfo, ServiceList) ->
    Filter = fun(#{ id := ServiceId, authz := AuthzConf} ) ->
                     Show = not maps:get(hide, AuthzConf, false),
                     Show orelse is_allowed(ServiceId, UserInfo, AuthzConf)
             end,
    {ok, lists:filter(Filter, ServiceList)}.

%% @doc update the list for the user with limits and permissions
-spec update_limits_for_user(watts_userinfo:info(), [info()])
                            -> {ok, [limited_info()]}.
update_limits_for_user(UserInfo, ServiceList) ->
    UpdateLimit
        = fun(Service, List) ->
                  #{ id := ServiceId,
                     cred_limit := Limit,
                     authz := AuthzConf
                   } = Service,
                  Tooltip = maps:get(tooltip, AuthzConf),
                  Authz = is_allowed(ServiceId, UserInfo, AuthzConf),
                  {ok, Count} = watts_plugin:get_count(UserInfo, ServiceId),
                  LimitReached = (Count >= Limit),
                  Update = #{ limit_reached => LimitReached,
                              cred_count => Count,
                              authorized => Authz,
                              authz_tooltip => Tooltip
                            },
                  Keys = [id, description, enabled, params,
                          pass_access_token, cred_limit,
                          cred_count, limit_reached,
                          authorized, authz_tooltip],
                  Entry = maps:with(Keys, maps:merge(Service, Update)),
                  [ Entry | List]
          end,
    {ok, lists:reverse(lists:foldl(UpdateLimit, [], ServiceList))}.



%% @doc get the info for a sepcific service id
-spec get_info(binary()) -> {ok, info()} | {error, Reason :: atom()}.
get_info(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, Info};
        Other -> Other
    end.

%% @doc get the credential limit for a service
-spec get_credential_limit(binary()) -> {ok, integer() | infinite}.
get_credential_limit(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, maps:get(cred_limit, Info, 0)};
        _ -> {ok, 0}
    end.

%% @doc return if a service with the given id exists
-spec exists(binary()) -> boolean().
exists(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, _} ->
            true;
        _ ->
            false
     end.

%% @doc get the queue name (an atom) for a service id
-spec get_queue(binary()) -> {ok, atom()}.
get_queue(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, {_Id, Info}} -> {ok, maps:get(queue, Info, undefined)};
        _ -> {ok, undefined}
    end.

%% @doc return if a service, given by the id, is enabled
-spec is_enabled(binary()) -> boolean().
is_enabled(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(enabled, Info, false);
        _ -> false
    end.


%% @doc wether the plugin may return the same state twice
-spec allows_same_state(binary()) -> boolean().
allows_same_state(ServiceId) ->
    case watts_ets:service_get(ServiceId) of
        {ok, {_Id, Info}} -> maps:get(allow_same_state, Info, false);
        _ -> false
    end.

%% @doc checks if a user is allowed to use the given service
%% @see is_allowed/3
-spec is_allowed(UserInfo :: watts_userinfo:userinfo(), ServiceId :: binary())
                -> boolean().
is_allowed(UserInfo, ServiceId) ->
    case get_info(ServiceId) of
        {ok, #{authz := AuthzConf} } ->
            is_allowed(ServiceId, UserInfo, AuthzConf);
        _ ->
            false
    end.

%% @doc checking the service authorization rules
%% see watts_service_authz
-spec is_allowed( ServiceId :: binary(), UserInfo :: watts_userinfo:userinfo(),
                  AuthzConf :: watts_service_authz:config())
                -> boolean().
is_allowed(ServiceId, UserInfo, AuthzConf) ->
    watts_service_authz:is_authorized(ServiceId, UserInfo, AuthzConf).


%% @doc check if the passed request parameters are valid for the service
-spec are_params_valid(map(), info() | limited_info() | binary()) -> boolean().
are_params_valid(Params, #{params := ParamSets})
  when is_map(Params) ->
    ToBinary = fun(Key, List) when is_binary(Key) ->
                       [ Key | List ];
                 (Atom, List) when is_atom(Atom) ->
                       [ atom_to_binary(Atom, utf8) | List ]
               end,
    ParamList = lists:foldl(ToBinary, [], maps:keys(Params)),
    ExistsMatching =
        fun(ParamSet, Current) ->
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


%% @doc check if the list of keys are fullfilling the given parameter set
-spec fulfills_paramset([binary()], parameter_set()) -> boolean().
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



%% @doc add a service to the ets database, used during initialization
-spec add(info()) -> {ok, binary()} | {error, invalid_config}.
add(#{ id := ServiceId } = ServiceInfo) when is_binary(ServiceId) ->
    AuthzConf0 = maps:get(authz, ServiceInfo, #{allow => [], forbid => []}),
    {ok, AuthzConf} = watts_service_authz:validate_config(ServiceId,
                                                          AuthzConf0),
    Update = #{enabled => false, authz => AuthzConf},
    ok = watts_ets:service_add(ServiceId, maps:merge(ServiceInfo, Update)),
    {ok, ServiceId};
add(_ServiceMap)  ->
    {error, invalid_config}.


%% @doc update the parameter of a service by running the plugin
-spec update_params(binary()) -> ok | {error, not_found}.
update_params(Id) ->
    Service = watts_ets:service_get(Id),
    get_and_validate_parameter(Service).

%% @doc get and validate the parameter from the plugin.
-spec get_and_validate_parameter({ok, {binary(), info()}} | any())
                                -> ok | {error, not_found}.
get_and_validate_parameter({ok, {Id, Info}}) ->
    Result = watts_plugin:get_params(Id),
    validate_params_and_update_db(Id, Info, Result);
get_and_validate_parameter(_) ->
    {error, not_found}.

%% @doc validate the parameter returned from the plugin and maybe update the ets
-spec validate_params_and_update_db(binary(), info(), {ok, map()} | any())
                                   -> ok | {error, not_found}.
validate_params_and_update_db(Id, Info, {ok, #{conf_params := ConfParams,
                                               request_params := RequestParams,
                                               version := Version}}) ->
    lager:info("service ~p: plugin version ~p", [Id, Version]),
    Ensure = #{plugin_conf => #{},
               params => [],
               plugin_version => Version
              },
    Info0 = maps:merge(Info, Ensure),
    {ValidConfParam, Info1}=validate_conf_parameter(ConfParams, Info0),
    {ValidCallParam, Info2}=validate_call_parameter_sets(RequestParams, Info1),
    Info3 = list_skipped_parameter_and_delete_config(Info2),
    IsValid = ValidConfParam and ValidCallParam,
    Update = #{enabled => IsValid},
    NewInfo = maps:merge(Info3, Update),
    {ok, QueueName} = start_runner_queue_if_needed(NewInfo),
    update_service(Id, maps:put(queue, QueueName, NewInfo));
validate_params_and_update_db(Id, _, {ok, ParamMap}) ->
    NeededKeys = [conf_params, request_params, version],
    MissingKeys = NeededKeys -- maps:keys(ParamMap),
    lager:error("service ~p: missing keys in parameter response: ~p",
                [Id, MissingKeys]),
    {error, bad_config};
validate_params_and_update_db(Id, _, {error, Result}) ->
    lager:error("service ~p: bad parameter response: ~p (from plugin)",
                [Id, Result]),
    {error, bad_config}.



%% @doc validate configuration parameter
-spec validate_conf_parameter(conf_parameters(), info()) -> {boolean(), info()}.
validate_conf_parameter(Params, Info) ->
    validate_conf_parameter(Params, Info, true).

%% @doc validate the configuration parameter and keep track of the result.
-spec validate_conf_parameter(conf_parameters(), info(), boolean())
                             -> {boolean(), info()}.
validate_conf_parameter([], Info, Result) ->
    {Result, Info};
validate_conf_parameter([#{name := Name, default := Def , type := Type } | T ],
                        Info, Current) ->
    ValidName = is_valid_key(Name),
    AtomType = to_conf_type(Type),
    Default = convert_to_type(Def, AtomType),
    {Res, NewInfo} = update_conf_parameter(Name, ValidName, Default,
                                           AtomType, Info),
    validate_conf_parameter(T, NewInfo, Res and Current);
validate_conf_parameter([ Entry | T ], #{ id:= Id, cmd:= Cmd}=Info, _Current) ->
    RequiredKeys = [name, default, type],
    MissingKeys =  RequiredKeys -- maps:keys(Entry),
    lager:error("service ~p: conf parameter missing keys ~p [~p]",
                [Id, MissingKeys, Cmd]),
    validate_conf_parameter(T, Info, false);
validate_conf_parameter(_, #{id := Id, cmd := Cmd} = Info, _) ->
    lager:error("service ~p: bad conf parameter for plugin ~p", [Id, Cmd]),
    {false, Info}.

%% @doc check the conversion results and update the config if all okay.
-spec update_conf_parameter(binary(), boolean(), {ok, any()} | {error, any()},
                            atom(), info()) -> {boolean(), info()}.
update_conf_parameter(Name, true, {ok, Default}, Type,
                      #{id := Id, plugin_conf_config:=RawConf,
                        plugin_conf:= Conf} = Info) when Type /= unknown ->
    UseDefault = not maps:is_key(Name, RawConf),
    ok = maybe_warn_default(UseDefault, Id, Name, Default),
    RawValue = maps:get(Name, RawConf, Default),
    {ok, Value} = convert_to_type(RawValue, Type),
    NewConf = maps:put(Name, Value, Conf),
    {true, maps:put(plugin_conf, NewConf, Info)};
update_conf_parameter(Name, _Valid, _Default, unknown, #{id := Id} = Info) ->
    Msg = "service ~p: unsupported datatype at conf parameter ~p (from plugin)",
    lager:error(Msg, [Id, Name]),
    {false, Info};
update_conf_parameter(Name, true, _, _Type, #{id := Id} = Info) ->
    lager:error("service ~p: bad default at conf parameter ~p (from plugin)",
                [Id, Name]),
    {false, Info};
update_conf_parameter(Name, false, _, _Type, #{id := Id} = Info) ->
    lager:error("service ~p: bad config parameter name '~p' (from plugin)",
                [Id, Name]),
    {false, Info}.

%% @doc print a warning if using the default value from the plugin
-spec maybe_warn_default(boolean(), binary(), binary(), any()) -> ok.
maybe_warn_default(true, Id, Name, Default) ->
    WMsg = "service ~p: plugin config ~p not set, using default: ~p",
    lager:warning(WMsg, [Id, Name, Default]),
    ok;
maybe_warn_default(false, _Id, _Name, _Default) ->
    ok.


%% @doc validate the call parameter sets
-spec validate_call_parameter_sets([parameter_set()], info())
                                  -> {boolean(), info()}.
validate_call_parameter_sets(Params, Info) ->
    validate_call_parameter_sets(Params, Info, true).

%% @doc validate the call parameter sets and keep track of the result
%% Itterate through each parameter set, validating it.
-spec validate_call_parameter_sets([parameter_set()], info(), boolean())
                                  -> {boolean(), info()}.
validate_call_parameter_sets([], #{params := []} =Info, Result) ->
    {Result, maps:put(params, [[]], Info)};
validate_call_parameter_sets([], #{params := Params} =Info, Result) ->
    {Result, maps:put(params, lists:reverse(Params), Info)};
validate_call_parameter_sets([ H | T ], Info, Current)
  when is_list(H) ->
    {Result, NewInfo} = validate_call_parameter_set(H, Info),
   validate_call_parameter_sets(T, NewInfo, Result and Current);
validate_call_parameter_sets([ H | T ], #{id := Id} = Info, _) ->
    lager:error("service ~p: bad request parameter set ~p (from plugin)",
                [Id, H]),
    validate_call_parameter_sets(T, Info, false).


%% @doc validate a single parameter set.
%% a singe set is a list of maps, each containing the specificaiton of
%% one parameter
-spec validate_call_parameter_set(parameter_set(), info())
                                 -> {boolean(), info()}.
validate_call_parameter_set(Set, Info) ->
    validate_call_parameter_set(Set, Info, [], [], true).

%% @doc validate a call parameter set and keep track of the result.
%% also update at the end the params list within the info with each new set.
-spec validate_call_parameter_set(parameter_set(), info(), parameter_set(),
                                  [binary()], boolean()) -> {boolean(), info()}.
validate_call_parameter_set([], Info, ParamSet, _Keys, Result)->
    Params = maps:get(params, Info),
    NewParams = [ ParamSet | Params ],
    NewInfo = maps:put(params, NewParams, Info),
    {Result, NewInfo};
validate_call_parameter_set([#{key := Key, name := Name, description := Desc,
                               type := Type} = Param | T], #{id := Id} = Info,
                            ParamSet, Keys, Current) ->
    ValidKey = is_valid_key(Key),
    KeyExists = lists:member(Key, Keys),
    {Result, NewParamSet}=validate_call_parameter(Key, ValidKey, KeyExists,
                                                  Name, Desc, Type, Param, Id,
                                                  ParamSet),
    validate_call_parameter_set(T, Info, NewParamSet, Keys, Current and Result);
validate_call_parameter_set([Param | T], #{id := Id} = Info, ParamSet, Keys, _)
  when is_map(Param) ->
    RequiredKeys = [description, name, key, type],
    MissingKeys = RequiredKeys -- maps:keys(Param),
    EMsg = "service ~p: request parameter ~p is missing keys ~p",
    lager:error(EMsg, [Id, Param, MissingKeys]),
    validate_call_parameter_set(T, Info, ParamSet, Keys, false);
validate_call_parameter_set([H | T], #{id := Id} = Info, ParamSet, Keys, _) ->
    EMsg = "service ~p: bad request parameter ~p (from plugin)",
    lager:error(EMsg, [Id, H]),
    validate_call_parameter_set(T, Info, ParamSet, Keys, false).


%% @doc validate one parameter for a call (part of a parameter set)
-dialyzer({nowarn_function, validate_call_parameter/9}).
-spec validate_call_parameter(binary(), boolean(), boolean(), binary(),
                              binary(), atom(), map(), binary(),
                              parameter_set()) -> {boolean(), parameter_set()}.
validate_call_parameter(Key, true, false, Name, Desc, Type, Param, Id,
                        ParamSet) when is_binary(Key), is_binary(Name),
                                       is_binary(Desc) ->
    EMsg = "service ~p: parameter ~p: bad type ~p (from plugin)",
    Mdtory = maps:get(mandatory, Param, false),
    {ok, Mandatory} = convert_to_type(Mdtory, boolean),
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
validate_call_parameter(Key, _, true, _N, _D, _T, Param, Id, ParamSet) ->
    EMsg = "service ~p: key ~p exists multiple times: ~p",
    lager:error(EMsg, [Id, Key, Param]),
    {false, ParamSet};
validate_call_parameter(Key, false, false, _N, _D, _T, Param, Id, ParamSet) ->
    EMsg = "service ~p: key ~p of parameter contains spaces: ~p",
    lager:error(EMsg, [Id, Key, Param]),
    {false, ParamSet};
validate_call_parameter(_,  true, false, _N, _D, _T, Param, Id, ParamSet) ->
    EMsg = "service ~p: bad request parameter values ~p (not strings)",
    lager:error(EMsg, [Id, Param]),
    {false, ParamSet}.




%% @doc list the skipped parameter from the config and delete the raw config
-spec list_skipped_parameter_and_delete_config(info()) -> info().
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


%% @doc start the runnuner queue if needed.
-spec start_runner_queue_if_needed(info()) -> {ok, atom()}.
start_runner_queue_if_needed(#{enabled := true,
                               parallel_runner := NumRunner,
                               id := Id
                              })
  when is_number(NumRunner) ->
    QueueId = gen_queue_name(Id),
    ok = add_queue(QueueId, NumRunner),
    Msg = "service ~p: queue ~p started with max ~p parallel runners",
    lager:info(Msg, [Id, QueueId, NumRunner]),
    {ok, QueueId} ;
start_runner_queue_if_needed(_) ->
    {ok, undefined}.

%% @doc add a queue to jobs
-spec add_queue(binary(), integer()) -> ok.
-dialyzer({nowarn_function, add_queue/2}).
add_queue(Id, NumRunner) ->
    Options = [{counter, [{limit, NumRunner}]}, {type, fifo}],
    ok = jobs:add_queue(Id, Options),
    ok.


%% @doc update the config of the service in the ets.
-spec update_service(binary(), info()) -> ok | {error, Reason :: atom()}.
update_service(Id, NewInfo) when is_map(NewInfo) ->
    watts_ets:service_update(Id, NewInfo).

%% @doc convert the valut to the given type.
-spec convert_to_type(binary(), atom())
                     -> {ok, binary() | atom()} | {error, bad_value}.
convert_to_type(Value, string)
  when is_binary(Value) ->
    {ok, Value};
convert_to_type(Value, boolean) ->
    TrueValues = [true, <<"True">>, <<"true">>, 1],
    IsTrue = lists:member(Value, TrueValues),
    FalseValues = [false, <<"False">>, <<"false">>, 0],
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


%% @doc convert the binary to an existing atom or unknown.
-spec to_atom(binary()) -> atom().
to_atom(Type) ->
    try
        binary_to_existing_atom(Type, utf8)
    catch error:badarg ->
            unknown
    end.

%% @doc convert the type to a configuration type.
-spec to_conf_type(binary()) -> atom().
to_conf_type(Type) ->
    ValidTypes = [boolean, string],
    to_valid_type(Type, ValidTypes).

%% @doc convert the type to a request type.
-spec to_request_type(binary()) -> atom().
to_request_type(Type) ->
    ValidTypes = [textarea],
    to_valid_type(Type, ValidTypes).

%% @doc try to convert the type to an atom and find it in the given list.
-spec to_valid_type(binary(), [atom()]) -> atom().
to_valid_type(Type, ValidTypes) ->
    AType = to_atom(Type),
    case lists:member(AType, ValidTypes) of
        true ->
            AType;
        _ ->
            unknown
    end.

%% @doc generate the name of a queue: the atom 'watts_service-ID'.
%% Where ID is the id of the service.
-spec gen_queue_name(binary()) -> atom().
gen_queue_name(Id) when is_binary(Id) ->
    Dash = <<"-">>,
    Module = atom_to_binary(?MODULE, utf8),
    QueueName = << Module/binary, Dash/binary, Id/binary >>,
    binary_to_atom(QueueName, utf8).

%% @doc return if a key is valid.
-spec is_valid_key(binary()) -> boolean().
is_valid_key(Key) when is_binary(Key) ->
    CharList = binary_to_list(Key),
    IsValid = fun(Char, Current) ->
                      is_valid_key_char(Char) and Current
              end,
    lists:foldl(IsValid, true, CharList).


%% @doc ceck if the character is valid within a key
-spec is_valid_key_char(integer()) -> boolean().
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
