-module(tts_service_authz).

-export([is_authorized/3]).
-export([validate_config/2]).


is_authorized(ServiceId, UserInfo, #{allow := Allow0, forbid := Forbid0} = A) ->
    {ok, Issuer, Subject} = watts_userinfo:return(issuer_subject, UserInfo),
    lager:debug("checking authorization of ~p ~p at service ~p [~p]",
                [Issuer, Subject, ServiceId, A]),
    try
        {ok, Pid} = oidcc:find_openid_provider(Issuer),
        {ok, #{id := ProviderId}} = oidcc:get_openid_provider_info(Pid),

        FilterById = fun({Id, _, _, _}) ->
                             case Id of
                                 ProviderId -> true;
                                 any -> true;
                                 _ -> false
                             end
                     end,
        Allow = lists:filter(FilterById, Allow0),
        Forbid = lists:filter(FilterById, Forbid0),
        lager:debug("running through allow ~p and forbid ~p", [Allow, Forbid]),
        Allowed = apply_rules(ServiceId, UserInfo, Allow, false),
        Forbidden = apply_rules(ServiceId, UserInfo, Forbid, true),
        Allowed and (not Forbidden)
    catch
        Error:Reason ->
            StackTrace = erlang:get_stacktrace(),
            EMsg = "Service ~p: Authzcheck for ~p failed due to ~p:~p at ~p",
            lager:error(EMsg, [ServiceId, UserInfo, Error, Reason, StackTrace]),
            false
     end.

apply_rules(ServiceId, UserInfo, Rules, Default) ->
    ApplyRule = fun(Rule, Current) ->
                        case Current of
                            true -> true;
                            false ->
                                apply_rule(ServiceId, Rule, UserInfo, Default)
                         end
                end,
    lists:foldl(ApplyRule, false, Rules).

apply_rule(_ServiceId, {_Iss, Key, Op, Value}, UserInfo, Default) ->
    case watts_userinfo:return({key, Key}, UserInfo) of
        {ok, KeyValue} ->
            Res = perform_operation(Op, KeyValue, Value),
            lager:debug("performed ~p(~p, ~p) -> ~p",
                        [Op, KeyValue, Value, Res]),
            Res;
        _ ->
            Default
    end.

perform_operation(any, _, Value) ->
    Value;
perform_operation(equals, KeyValue, Value) ->
    (KeyValue == Value);
perform_operation(is_member_of, KeyValue, Value)
  when is_list(Value) ->
    lists:member(KeyValue, Value);
perform_operation(contains, KeyValue, Value)
  when is_list(KeyValue), is_binary(Value) ->
    lists:member(Value, KeyValue);
perform_operation(contains, KeyValue, Value)
  when is_binary(KeyValue), is_binary(Value) ->
    case binary:match(KeyValue, [Value]) of
        {_, _} ->
            true;
        _ ->
            false
    end;
perform_operation(regexp, KeyValue, Value) ->
    case re:run(KeyValue, Value, []) of
        {match, _} ->
            true;
        _ ->
            false
    end.




validate_config(ServiceId, #{allow := Allow0, forbid := Forbid0} = Authz0) ->
    lager:info("Service ~p: validating authz", [ServiceId]),
    {AllowOk, Allow} = validate(Allow0),
    lager:info("Service ~p: validating authz forbid", [ServiceId]),
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

validate(List) ->
    {ok, ProviderList} = oidcc:get_openid_provider_list(),
    validate(List, ProviderList, {true, []}).


validate([], _ProviderList, Result) ->
    Result;
validate([{ProviderId, OidcKey, Operation, Value} | T], ProviderList, Result) ->
    ProviderExists = does_provider_exist(ProviderId, ProviderList),
    NewResult = maybe_add_to_result(ProviderExists, Operation, OidcKey, Value,
                                    Result),
    validate( T, ProviderList, NewResult ).


maybe_add_to_result({true, Issuer}, AtomOp, OidcKey, Value, {Result, List}) ->
    {Result, [ {Issuer, OidcKey, AtomOp, Value} | List ]} ;
maybe_add_to_result({false, Id}, Op, Key, Val, Result) ->
    add_failed("provider not found", Key, Op, Val, Id, Result).

add_failed(Reason, Key, Op, Val, Id, {Result, List}) ->
    Msg = "bad rule ~p ~p ~p for provider ~p (~s) -> ~p",
    lager:warning(Msg, [Key, Op, Val, Id, Reason, Result]),
    {false, List}.


does_provider_exist(ProviderId, ProviderList) ->
    case {lists:keyfind(ProviderId, 1, ProviderList), ProviderId == any} of
        {false, false} ->
            {false, undefined};
        {false, true} ->
            {true, any};
        {_, false} ->
            {true, ProviderId}
    end.
