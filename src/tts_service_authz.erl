-module(tts_service_authz).

-export([is_authorized/3]).
-export([validate_config/2]).


is_authorized(ServiceId, UserInfo, #{allow := Allow0, forbid := Forbid0}) ->
    {ok, Issuer} = tts_userinfo:return(issuer, UserInfo),
    FilterByIssuer = fun({Iss, _, _, _}) ->
                             Iss == Issuer
                     end,
    Allow = lists:filter(FilterByIssuer, Allow0),
    Forbid = lists:filter(FilterByIssuer, Forbid0),
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
            perform_operation(Op, KeyValue, Value, Default);
        _ ->
            Default
    end;
apply_rule(ServiceId, Rule, _UserInfo, Default) ->
    lager:critical("Service ~p: rule not understood: ~p, setting to ~p",
                   [ServiceId, Rule, Default]),
    Default.

perform_operation(any = Op, KeyValue, Value, _) ->
    Res = true,
    lager:debug("performing ~p(~p, ~p) -> ~p",[Op, KeyValue, Value, Res]),
    Res;
perform_operation(equals = Op, KeyValue, Value, _) ->
    Res = (KeyValue == Value),
    lager:debug("performing ~p(~p, ~p) -> ~p",[Op, KeyValue, Value, Res]),
    Res;
perform_operation(contains = Op, KeyValue, Value, _)
  when is_list(KeyValue), is_binary(Value) ->
    Res = lists:member(Value, KeyValue),
    lager:debug("performing ~p(~p, ~p) -> ~p",[Op, KeyValue, Value, Res]),
    Res;
perform_operation(contains = Op, KeyValue, Value, _)
  when is_binary(KeyValue), is_binary(Value) ->
    Res = case binary:match(KeyValue, [Value]) of
              {_, _} ->
                  true;
              _ ->
                  false
          end,
    lager:debug("performing ~p(~p, ~p) -> ~p",[Op, KeyValue, Value, Res]),
    Res;
perform_operation(regexp = Op, KeyValue, Value, _) ->
    Res = case re:run(KeyValue, Value, []) of
              {match, _} ->
                  true;
              _ ->
                  false
          end,
    lager:debug("performing ~p(~p, ~p) -> ~p",[Op, KeyValue, Value, Res]),
    Res;
perform_operation(Op, KeyValue, Value, Default) ->
    lager:critical("unknwon operation/value combination: ~p(~p, ~p), using ~p",
                   [Op, KeyValue, Value, Default]),
    Default.




validate_config(ServiceId, #{allow := Allow0, forbid := Forbid0}) ->
    {ok, ProviderList} = oidcc:get_openid_provider_list(),
    lager:info("Service ~p: validating authz allow", [ServiceId]),
    Allow = validate(Allow0, ProviderList, []),
    lager:info("Service ~p: validating authz forbid", [ServiceId]),
    Forbid = validate(Forbid0, ProviderList, []),
    ValidatedAuthz = #{allow => Allow, forbid => Forbid},
    lager:debug("Service ~p: authz result ~p", [ServiceId, ValidatedAuthz]),
    {ok, ValidatedAuthz}.

validate([], _ProviderList, Result) ->
    Result;
validate([{ProviderId, OidcKey, Operation, Value} | T], ProviderList, Result) ->
    ProviderExists = does_provider_exist(ProviderId, ProviderList),
    ConvertedOp = convert_operation(Operation),
    NewResult = maybe_add_to_result(ProviderExists, ConvertedOp, OidcKey,
                                 Value, Result),
    validate( T, ProviderList, NewResult ).


-define(OPERATIONS,
        [
         {<<"contains">>, contains},
         {<<"equals">>, equals},
         {<<"regexp">>, regexp},
         {<<"any">>, any}
        ]).

convert_operation(Operation) ->
    case lists:keyfind(Operation, 1, ?OPERATIONS) of
        {Operation, AtomOp} ->
            {true, AtomOp};
        _ ->
            {false, Operation}
    end.


maybe_add_to_result({true, Issuer}, {true, AtomOp}, OidcKey, Value0, Result) ->
    case convert_value_if_needed(AtomOp, Value0) of
        {ok, Value} ->
            [ {Issuer, OidcKey, AtomOp, Value} | Result ] ;
        {error, Reason} ->
            Msg = "skipping rule ~p ~p ~p for issuer ~p (~p)",
            lager:warning(Msg, [OidcKey, AtomOp, Value0, Issuer, Reason]),
            Result
    end;
maybe_add_to_result({false, Id}, {_, Op}, Key, Val, Result) ->
    Msg = "skipping rule ~p ~p ~p for provider id ~p (provider not found)",
    lager:warning(Msg, [Key, Op, Val, Id]),
    Result;
maybe_add_to_result({true, Id}, {false, Op}, Key, Val, Result) ->
    Msg = "skipping rule ~p ~p ~p for issuer ~p (unknown operation)",
    lager:warning(Msg, [Key, Op, Val, Id]),
    Result;
maybe_add_to_result(Id, Op, Key, Val, Result) ->
    Msg = "skipping rule ~p ~p ~p for provider ~p (unknown error)",
    lager:warning(Msg, [Key, Op, Val, Id]),
    Result.

does_provider_exist(ProviderId, ProviderList) ->
    case lists:keyfind(ProviderId, 1, ProviderList) of
        false ->
            {false, ProviderId};
        _ ->
            {ok, #{issuer := Iss}} = oidcc:get_openid_provider_info(ProviderId),
            {true, Iss}
    end.


convert_value_if_needed(any, _) ->
    {ok, undefined};
convert_value_if_needed(regexp, Value) ->
    case re:compile(Value) of
        {ok, MP} ->
            {ok, MP};
        {error, Reason} ->
            Msg = io_lib:format("compilation of regexp failed: ~p",[Reason]),
            {error, Msg}
    end;
convert_value_if_needed(_, Value) ->
    {ok, Value}.
