-module(watts_http_util).
-include("watts.hrl").
-export([
         perform_cookie_action/4,
         create_cookie_opts/1,
         cookie_name/0,
         relative_path/1,
         whole_url/1
        ]).

-define(COOKIE, <<"watts_session">>).

perform_cookie_action(clear, _MaxAge, _Value, Req) ->
    Opts = create_cookie_opts(0),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, <<"deleted">>, Opts, Req),
    {ok, Req2};
perform_cookie_action(update, 0, _, Req) ->
    perform_cookie_action(clear, 0, deleted, Req);
perform_cookie_action(update, _, deleted, Req) ->
    perform_cookie_action(clear, 0, deleted, Req);
perform_cookie_action(update, MaxAge, Value, Req) ->
    Opts = create_cookie_opts(MaxAge),
    Req2 = cowboy_req:set_resp_cookie(?COOKIE, Value, Opts, Req),
    {ok, Req2}.

create_cookie_opts(MaxAge) ->
    BasicOpts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    case ?CONFIG(ssl) of
        true ->
            [{secure, true} | BasicOpts];
        _ ->
            BasicOpts
    end.

relative_path(Append) ->
    AppendBin = list_to_binary(Append),
    Base = ?CONFIG(ep_main),
    << Base/binary, AppendBin/binary >>.

whole_url(Path) when is_binary(Path)->
    HostName = binary:list_to_bin(?CONFIG(hostname)),
    Prot = local_protocol(),
    Port = local_port(),
    << Prot/binary, HostName/binary, Port/binary, Path/binary >>;
whole_url(Path) when is_list(Path) ->
    whole_url(list_to_binary(Path)).


cookie_name() ->
    ?COOKIE.

local_port() ->
    return_port(?CONFIG(port), ?CONFIG(ssl)).
return_port(443, true) ->
    <<"">>;
return_port(80, false) ->
    <<"">>;
return_port(Port, _) ->
    PortBin = binary:list_to_bin(io_lib:format("~p", [Port])),
    << <<":">>/binary, PortBin/binary>>.


local_protocol() ->
    return_http(?CONFIG(ssl)).
return_http(false) ->
    <<"http://">>;
return_http(_) ->
    <<"https://">>.
