%% @doc This module handles the authorization of users, defined by their
%% sessions, to request credentials at specific services. So it allows
%% users the trigger the run of a plugin or not.
%%
%% Each service has two configuration options:
%% <ul>
%% <li> the list of allowed users </li>
%% <li> the list of forbidden users </li>
%% </ul>
%% Each entry is not a list of users but a rule to identify certain users by
%% OpenID Connect information.
%%
%% A user is allowed if at least one rule of the allow evaluates to true and
%% none of the forbidden rules evaluate to true. So by default all users are
%% forbidden.

-module(watts_service_authz).
-include("watts.hrl").
-export([is_authorized/3]).
-export([validate_config/2]).

-export_type([config/0]).

-type operation() :: contains | is_member_of | equals | regexp | any.
-type value() :: binary() | [binary()] | re:mp() | boolean().
-type rule() :: {ProviderId :: binary(),
                 Key :: binary(),
                 Operation :: operation(),
                 ConfigValue :: value()
                }.
-type rules() :: [ rule() ].
-type config() :: #{allow => rules(), forbid => rules()}.



%% @doc checks and returns if a user is allowed to use a service.
-spec is_authorized(ServiceId :: binary(),
                    UserInfo :: watts_userinfo:userinfo(),
                    Config :: config()) -> boolean().
is_authorized(ServiceId, UserInfo, #{allow := Allow0, forbid := Forbid0} = A) ->
    {ok, Issuer, Subject} = watts_userinfo:return(issuer_subject, UserInfo),
    lager:debug("checking authorization of ~p ~p at service ~p [~p]",
                [Issuer, Subject, ServiceId, A]),
    try
        {ok, ProviderId} = get_provider_id(Issuer),
        {Allow, Forbid} = limit_rules_to_provider(Allow0, Forbid0, ProviderId),
        lager:debug("running through allow ~p and forbid ~p", [Allow, Forbid]),
        Allowed = any_rule_applies(UserInfo, Allow, false),
        Forbidden = any_rule_applies(UserInfo, Forbid, true),
        Allowed and (not Forbidden)
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            EMsg = "Service ~p: Authzcheck for ~p failed due to ~p:~p at ~p",
            lager:error(EMsg, [ServiceId, UserInfo, Error, Reason, StackTrace]),
            false
     end.

%% @doc limit the rules to those that apply to the provider given
-spec limit_rules_to_provider(Allow :: rules(), Frobid :: rules(),
                              ProviderId :: binary()) -> {rules(), rules()}.
limit_rules_to_provider(Allow0, Forbid0, ProviderId) ->
    FilterByProviderId = fun({Id, _, _, _}) ->
                                 (Id == ProviderId) or (Id == any)
                         end,
    Allow = lists:filter(FilterByProviderId, Allow0),
    Forbid = lists:filter(FilterByProviderId, Forbid0),
    {Allow, Forbid}.


%% @doc apply a list of rules on a user.
%% this will either evaluate to true or false and so either
%% allow or forbid the user.
%% The rules are evaluated in an 'or' expression. So if
%% at least one rule evaluates to true the whole rule-list
%% is true, false otherwise.
-spec any_rule_applies(UserInfo :: watts_userinfo:userinfo(),
                  Rules :: rules(),
                  Default :: boolean()
                 ) -> boolean().
any_rule_applies(UserInfo, Rules, Default) ->
    ApplyRule = fun(Rule, Current) ->
                        case Current of
                            true -> true;
                            false ->
                                apply_rule(Rule, UserInfo, Default)
                         end
                end,
    lists:foldl(ApplyRule, false, Rules).



%% @doc Get UserValue and run the operation, Default returned on error.
-spec apply_rule(Rule :: rule(),
                 UserInfo :: watts_userinfo:userinfo(),
                 Default :: boolean() ) -> boolean().
apply_rule({_Iss, Key, Op, ConfigValue}, UserInfo, Default) ->
    case watts_userinfo:return({key, Key}, UserInfo) of
        {ok, UserValue} ->
            Res = perform_operation(Op, UserValue, ConfigValue),
            lager:debug("performed ~p(~p, ~p) -> ~p",
                        [Op, UserValue, ConfigValue, Res]),
            Res;
        _ ->
            Default
    end.


%% @doc perform the given operation.
%% A the moment the following operations are supported:
%% <ul>
%% <li> any: returns the configured boolean value </li>
%% <li> equals: returns if the config and userinfo value are equal </li>
%% <li> is_member_of: checks if user value is part of the config list </li>
%% <li> contains: checks if the config value is contained in the user value</li>
%% <li> regexp: checks the user value against a regular expression </li>
%% </ul>
-spec perform_operation(Operation :: operation(),
                        UserValue :: value(),
                        ConfigValue :: value()) ->
                               boolean().
perform_operation(any, _, ConfigValue) ->
    ConfigValue;
perform_operation(equals, UserValue, ConfigValue) ->
    (UserValue == ConfigValue);
perform_operation(is_member_of, UserValue, ConfigValue)
  when is_list(ConfigValue) ->
    lists:member(UserValue, ConfigValue);
perform_operation(contains, UserValue, ConfigValue)
  when is_list(UserValue), is_binary(ConfigValue) ->
    lists:member(ConfigValue, UserValue);
perform_operation(contains, UserValue, ConfigValue)
  when is_binary(UserValue), is_binary(ConfigValue) ->
    case binary:match(UserValue, [ConfigValue]) of
        {_, _} ->
            true;
        _ ->
            false
    end;
perform_operation(regexp, UserValue, ConfigValue) ->
    case re:run(UserValue, ConfigValue, []) of
        {match, _} ->
            true;
        _ ->
            false
    end.


%% @doc validate the authz config of a service.
-spec validate_config(ServiceId :: binary(), Config::config())
                     -> {ok, ValidatedConfig :: config()}.
validate_config(ServiceId, #{allow := Allow0, forbid := Forbid0} = Authz0) ->
    lager:info("Service ~p: validating authz: allow", [ServiceId]),
    {AllowOk, Allow} = validate(Allow0),
    lager:info("Service ~p: validating authz: forbid", [ServiceId]),
    {ForbidOk, Forbid} = validate(Forbid0),
    ValidatedAuthz =
        case AllowOk and ForbidOk of
            true ->
                maps:merge(Authz0, #{allow => Allow, forbid => Forbid});
            _ ->
                lager:critical("Service ~p: *DISABLED* (authz config errors)",
                               [ServiceId]),
                maps:merge(Authz0, #{allow => [], forbid => []})
        end,
    lager:debug("Service ~p: authz result ~p", [ServiceId, ValidatedAuthz]),
    {ok, ValidatedAuthz}.

%% @doc validate a rule list
-spec validate(Rules :: rules()) ->
                      {Failed :: boolean(), ValidatedRules :: rules()}.
validate(Rules) ->
    {ok, ProviderList} = oidcc:get_openid_provider_list(),
    {ok, RspProviderList} = get_rsp_provider_list(),
    AllProviderList = RspProviderList ++ ProviderList,
    validate(Rules, AllProviderList, {true, []}).

%% @doc get the list of RPS provider.
-spec get_rsp_provider_list() -> {ok,  [ {binary(), rsp} ]}.
get_rsp_provider_list() ->
    {ok, RspList} = watts_rsp:get_list(),
    Convert = fun(Rsp, Ids) ->
                      {ok, Id} = watts_rsp:get_id(Rsp),
                      Prefix = <<"rsp-">>,
                      ProviderId = << Prefix/binary, Id/binary >>,
                      [ {ProviderId, rsp} | Ids]
              end,
    {ok, lists:foldl(Convert, [], RspList)}.

%% @doc validate a list of rules ensuring all provider exist.
-spec validate(rules(), ProviderList :: list(), Result :: {boolean(), rules()})
              -> {Failed :: boolean(), ValidatedRules ::rules()}.
validate([], _ProviderList, Result) ->
    Result;
validate([{ProviderId, OidcKey, Operation, Value} | T], ProviderList, Result) ->
    ProviderExists = does_provider_exist(ProviderId, ProviderList),
    NewResult = maybe_add_to_result(ProviderExists, Operation, OidcKey, Value,
                                    Result),
    validate( T, ProviderList, NewResult ).

%% @doc only add a rule if a provider for the given role exists
-spec maybe_add_to_result({Add :: boolean(),
                           ProviderId :: binary() | any | rsp},
                          Operation :: operation(),
                          OidcKey :: binary(),
                          Value :: value(),
                          {Result :: boolean(), List :: rules()})
                         -> {IsOkay :: boolean(), Rules :: rules()}.
maybe_add_to_result({true, ProviderId}, Op, OidcKey, Value, {Result, List}) ->
    {Result, [ {ProviderId, OidcKey, Op, Value} | List ]} ;
maybe_add_to_result({false, Id}, Op, Key, Val, Result) ->
    add_failed("provider not found", Key, Op, Val, Id, Result).

%% @doc error message and failing the rules.
-spec add_failed(string(), any(), any(), any(), any(), {boolean(), rules()})
                 -> {false, rules()}.
add_failed(Reason, Key, Op, Val, Id, {Result, List}) ->
    Msg = "bad rule ~p ~p ~p for provider ~p (~s) -> ~p",
    lager:warning(Msg, [Key, Op, Val, Id, Reason, Result]),
    {false, List}.


%% @doc checks if a provider exists and if the provider is 'any'
-spec does_provider_exist(ProviderId :: binary() | any,
                          ProviderList :: [tuple()]) ->
                                 {false, undefined} |
                                 {true, any} |
                                 {true, rsp} |
                                 {true, binary()}.
does_provider_exist(ProviderId, ProviderList) ->
    case {lists:keyfind(ProviderId, 1, ProviderList), ProviderId == any} of
        {false, false} ->
            {false, undefined};
        {false, true} ->
            {true, any};
        {_, false} ->
            {true, ProviderId}
    end.

%% @doc return the primary provider whith which the user logged in
-spec get_provider_id(ProviderName :: binary()) -> {ok, binary()} |
                                                   {error, Reason :: atom()}.
get_provider_id(<< Prefix:4/binary, _/binary>> = Rsp)
  when Prefix == <<"rsp-">> ->
    Exists = watts_rsp:exists(Rsp),
    Enabled = ?CONFIG(enable_rsp),
    return_rsp_if_enabled_and_exists(Rsp, Exists, Enabled);
get_provider_id(Issuer) ->
    Result = oidcc:find_openid_provider(Issuer),
    return_provider_id_if_found(Result).


%% @doc return the provider if passed a valid pid
-spec return_provider_id_if_found({ok, pid} | any())
                                 -> {ok, binary()} | {error, provider_not_found}.
return_provider_id_if_found({ok, Pid}) ->
    {ok, #{id := ProviderId}} = oidcc:get_openid_provider_info(Pid),
    {ok, ProviderId};
return_provider_id_if_found(_) ->
    {error, provider_not_found}.



%% @doc return the rsp id, if RSP are enabled and it does exist.
-spec return_rsp_if_enabled_and_exists(RspId :: binary(), boolean(), boolean())
                                 -> {ok, binary()} | {error, rsp_disabled}.
return_rsp_if_enabled_and_exists(Rsp, true, true) ->
    {ok, Rsp};
return_rsp_if_enabled_and_exists(_, _, false) ->
    {error, rsp_disabled};
return_rsp_if_enabled_and_exists(_, false, _) ->
    {error, provider_not_found}.
