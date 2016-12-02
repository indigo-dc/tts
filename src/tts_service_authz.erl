-module(tts_service_authz).

-export([is_authorized/3]).
-export([validate_config/2]).


is_authorized(ServiceId, UserInfo, #{allow := Allow0, forbid := Forbid0} = A) ->
    {ok, Issuer, Subject} = tts_userinfo:return(issuer_subject, UserInfo),
    lager:debug("checking authorization of ~p ~p at service ~p [~p]",
                [Issuer, Subject, ServiceId, A]),

    Slash = <<"/">>,

    FilterByIssuer = fun({Iss, _, _, _}) ->
                             IssSlash = << Iss/binary, Slash/binary >>,
                             (Iss == Issuer) or (IssSlash == Issuer)
                                 or (Iss == any)
                     end,
    Allow = lists:filter(FilterByIssuer, Allow0),
    Forbid = lists:filter(FilterByIssuer, Forbid0),
    lager:debug("running through allow ~p and forbid ~p", [Allow, Forbid]),
    Allowed = apply_rules(ServiceId, UserInfo, Allow, false),
    Forbidden = apply_rules(ServiceId, UserInfo, Forbid, true),
    Allowed and (not Forbidden).

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
    case tts_userinfo:return({key, Key}, UserInfo) of
        {ok, KeyValue} ->
            Res = perform_operation(Op, KeyValue, Value, Default),
            lager:debug("performed ~p(~p, ~p) -> ~p",
                        [Op, KeyValue, Value, Res]),
            Res;
        _ ->
            Default
    end;
apply_rule(ServiceId, Rule, _UserInfo, Default) ->
    lager:critical("Service ~p: rule not understood: ~p, setting to ~p",
                   [ServiceId, Rule, Default]),
    Default.

perform_operation(any, _, Value, _) ->
    Value;
perform_operation(equals, KeyValue, Value, _) ->
    (KeyValue == Value);
perform_operation(is_member_of, KeyValue, Value, _)
  when is_list(Value) ->
    lists:member(KeyValue, Value);
perform_operation(contains, KeyValue, Value, _)
  when is_list(KeyValue), is_binary(Value) ->
    lists:member(Value, KeyValue);
perform_operation(contains, KeyValue, Value, _)
  when is_binary(KeyValue), is_binary(Value) ->
    case binary:match(KeyValue, [Value]) of
        {_, _} ->
            true;
        _ ->
            false
    end;
perform_operation(regexp, KeyValue, Value, _) ->
    case re:run(KeyValue, Value, []) of
        {match, _} ->
            true;
        _ ->
            false
    end;
perform_operation(Op, KeyValue, Value, Default) ->
    lager:critical("unknwon operation/value combination: ~p(~p, ~p), using ~p",
                   [Op, KeyValue, Value, Default]),
    Default.




validate_config(ServiceId, #{allow := Allow0, forbid := Forbid0}) ->
    lager:info("Service ~p: validating authz", [ServiceId]),
    {AllowOk, Allow} = validate(Allow0),
    lager:info("Service ~p: validating authz forbid", [ServiceId]),
    {ForbidOk, Forbid} = validate(Forbid0),
    ValidatedAuthz =
        case AllowOk and ForbidOk of
            true ->
                #{allow => Allow, forbid => Forbid};
            _ ->
                lager:critical("Service ~p: *DISABLED* (authz config errors)",
                               [ServiceId]),
                #{allow => [], forbid => []}
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
    add_failed("provider not found", Key, Op, Val, Id, Result);
maybe_add_to_result(Id, Op, Key, Val, Result) ->
    add_failed("unknown error", Key, Op, Val, Id, Result).

add_failed(Reason, Key, Op, Val, Id, {Result, List}) ->
    Msg = "bad rule ~p ~p ~p for provider ~p (~s) -> ~p",
    lager:warning(Msg, [Key, Op, Val, Id, Reason, Result]),
    {false, List}.


does_provider_exist(ProviderId, ProviderList) ->
    case lists:keyfind(ProviderId, 1, ProviderList) of
        false ->
            {false, ProviderId};
        _ ->
            {ok, #{issuer := Iss}} = oidcc:get_openid_provider_info(ProviderId),
            {true, Iss}
    end.
