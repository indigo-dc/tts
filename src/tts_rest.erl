-module(tts_rest).



-export([dispatch_mapping/1]).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([malformed_request/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-record(state, {
          version = undefined,
          type = undefined,
          id = undefinde
         }).

rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

%
% list of API methods:
% GET /oidcps/
% GET /oidc/$ID
% GET /services/
% GET /service/$ID
% GET /credentials
% POST /credentials
% GET /credential/$ID

dispatch_mapping(InBasePath) ->
    BasePath = case binary:last(InBasePath) of
                   $/ ->
                       Len = binary:byte_size(InBasePath),
                       binary:part(InBasePath, 0, Len-1);
                   _ ->
                       InBasePath
               end,
    << BasePath/binary, <<"/[:version/]:type/[:id]">>/binary >>.


-define(LATEST_VERSION, 1).

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

is_authorized(Req, State) ->
    {true, Req, State}.

malformed_request(Req, State) ->
    {InVersion, Req2} = cowboy_req:binding(version, Req, latest),
    {InType, Req3} = cowboy_req:binding(type, Req2),
    {InId, Req4} = cowboy_req:binding(id, Req3, none),
    {Result, NewState} = is_malformed(InVersion, InType, InId, State),
    {Result, Req4, NewState}.


get_html(Req, #state{version=Version, type=Type, id=Id}=State) ->
    Result = perform_get(Version, Type, Id),
    {Result, Req, State}.


perform_get(_Version, services, none) ->
    {ok, ServiceList} = tts_service:get_list(),
    return_service_list(ServiceList);
perform_get(_Version, credentials, none) ->
    jsx:encode([]);
perform_get(_Version, oidcps, none) ->
    {ok, OIDCList} = oidcc:get_openid_provider_list(),
    return_oidc_list(OIDCList);
perform_get(_Version, oidcps, Id) ->
    case oidcc:get_openid_provider_info(Id) of
        {ok, Info} ->
            %TODO: extract info
            io_lib:format("~p: ~p", [Id, Info]);
        _ -> io_lib:format("not_found")
    end;
perform_get(Version, Type, Id) ->
    io_lib:format("not yet implemented~nversion: ~p, type: ~p, id:~p", [Version,
                                                                        Type,
                                                                        Id]).

return_service_list(Services) ->
    Extract = fun(Map, List) ->
                      Entry = #{ id => maps:get(id, Map),
                                 type => maps:get(type, Map),
                                 host => maps:get(host, Map)
                               },
                      [Entry | List]
              end,
    List = lists:reverse(lists:foldl(Extract, [], Services)),
    jsx:encode(List).

return_oidc_list(Oidc) ->
    Id = fun({OidcId, _Pid}, List) ->
                 [OidcId | List]
         end,
    List = lists:reverse(lists:foldl(Id, [], Oidc)),
    jsx:encode(List).


is_malformed(InVersion, InType, InId, State) ->
    Version = verify_version(InVersion),
    Type = verify_type(InType),
    Id = verify_id(InId),
    Result = is_malformed(Version, Type, Id),
    {Result, State#state{version=Version, type=Type, id=Id}}.

verify_version(latest) ->
    ?LATEST_VERSION;
verify_version(<<"latest">>) ->
    verify_version(latest);
verify_version(<< V:1/binary, Version/binary >>) when V==<<"v">>; V==<<"V">> ->
    safe_binary_to_integer(Version);
verify_version(_) ->
    0.

safe_binary_to_integer(Version) ->
    try binary_to_integer(Version) of
        Number -> Number
    catch
        _:_ ->
            0
    end.



-define(TYPE_MAPPING, [
                       {<<"service">>, service},
                       {<<"op_provider">>, op_provider},
                       {<<"credential">>, credential}
                      ]).

verify_type(Type) ->
    case lists:keyfind(Type, 1, ?TYPE_MAPPING) of
        false -> unknown;
        {Type, AtomType} -> AtomType
    end.

verify_id(<<"list">>) ->
    list;
verify_id(Id) ->
    Id.

is_malformed(Version, _, _) when Version =< 0 ->
    true;
is_malformed(Version, _, _) when Version > ?LATEST_VERSION ->
    true;
is_malformed(_, unknown, _) ->
    true;
is_malformed(_, _, _) ->
    false.


