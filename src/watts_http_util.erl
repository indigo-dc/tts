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

%% @doc either update or delete a cookie.
%% the cookie will either be updated with a new lifetime or will be deleted
%% by setting the time to timestamp 0.
-spec perform_cookie_action(Action :: clear | update, MaxAge :: integer(),
                            CookieData :: deleted | binary(),
                            cowboy_req:req()) -> {ok, cowboy_req:req()}.
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

%% @doc generate the options list for the cookie
-spec create_cookie_opts(MaxAge :: integer()) -> [tuple()].
create_cookie_opts(MaxAge) ->
    BasicOpts = [ {http_only, true}, {max_age, MaxAge}, {path, <<"/">>}],
    case ?CONFIG(ssl) of
        true ->
            [{secure, true} | BasicOpts];
        _ ->
            BasicOpts
    end.

%% @doc create a relative path starting from main path (for web).
-spec relative_path(Append :: binary()) -> WholePath :: binary().
relative_path(Append) ->
    AppendBin = list_to_binary(Append),
    Base = ?CONFIG(ep_main),
    << Base/binary, AppendBin/binary >>.

%% @doc convert an absolute path into the url to that path
-spec whole_url(AbsPath :: string() | binary()) -> binary().
whole_url(AbsPath) when is_binary(AbsPath)->
    HostName = binary:list_to_bin(?CONFIG(hostname)),
    Prot = local_protocol(),
    Port = local_port(),
    << Prot/binary, HostName/binary, Port/binary, AbsPath/binary >>;
whole_url(AbsPath) when is_list(AbsPath) ->
    whole_url(list_to_binary(AbsPath)).

%% @doc create a redirection for cowboy to the given url.
-spec redirect_to(Url :: binary(), cowboy_req:req()) ->
                         {ok, cowboy_req:req(), []}.
redirect_to(Url, Req) ->
    Header = [{<<"location">>, Url}],
    {ok, Req2} = cowboy_req:reply(302, Header, Req),
    {ok, Req2, []}.

%% @doc return the name of the cookie.
-spec cookie_name() -> binary().
cookie_name() ->
    ?COOKIE.

%% @doc return the string for the current port setting
-spec local_port() -> PortText :: binary().
local_port() ->
    return_port(?CONFIG(port), ?CONFIG(ssl)).

%% @doc helper function to decide if the port needs to be written.
-spec return_port(PortNumber :: pos_integer(), UsingSSL :: boolean() )
                 -> PortText :: binary().
return_port(443, true) ->
    <<"">>;
return_port(80, false) ->
    <<"">>;
return_port(Port, _) ->
    PortBin = binary:list_to_bin(io_lib:format("~p", [Port])),
    << <<":">>/binary, PortBin/binary>>.

%% @doc return the protocol to use, either https or http.
-spec local_protocol() -> ProtocolText :: binary().
local_protocol() ->
    return_http(?CONFIG(ssl)).

%% @doc helper function to return the protocol
-spec return_http(UseSSL :: boolean()) -> binary().
return_http(false) ->
    <<"http://">>;
return_http(_) ->
    <<"https://">>.
