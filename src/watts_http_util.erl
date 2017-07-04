-module(watts_http_util).
%%
%% Copyright 2016 - 2017 SCC/KIT
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
-include("watts.hrl").
-export([
         perform_cookie_action/4,
         create_cookie_opts/1,
         cookie_name/0,
         relative_path/1,
         whole_url/1,
         redirect_to/2
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

redirect_to(Url, Req) ->
    Header = [{<<"location">>, Url}],
    {ok, Req2} = cowboy_req:reply(302, Header, Req),
    {ok, Req2, []}.

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
