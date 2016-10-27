-module(tts_init).
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
-behaviour(gen_server).

-include("tts.hrl").

%% API.
-export([start_link/0]).
-export([stop/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
         }).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


%% gen_server.

init([]) ->
    gen_server:cast(self(), add_oidc),
    {ok, #state{}, 1}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(add_oidc, State) ->
    add_openid_provider(),
    gen_server:cast(self(), add_services),
    {noreply, State};
handle_cast(add_services, State) ->
    gen_server:cast(self(), start_http),
    {noreply, State};
handle_cast(start_http, State) ->
    start_web_interface(),
    gen_server:cast(self(), stop),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, _State) ->
    % stop depending services
    {stop, normal, #state{}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_openid_provider() ->
    LocalEndpoint = local_endpoint(),
    ProviderList = ?CONFIG(provider_list, []),
    ok = add_openid_provider(ProviderList, LocalEndpoint),
    ok.

add_openid_provider([#{id := Id, description := Desc, client_id := ClientId,
                       client_secret := Secret, scopes := RequestScopes,
                       config_endpoint := ConfigEndpoint}|T], LocalEndpoint) ->
    {ok, _InternalId, _Pid} =
        oidcc:add_openid_provider(Id, Id, Desc, ClientId, Secret,
                                  ConfigEndpoint, LocalEndpoint, RequestScopes),
    add_openid_provider(T, LocalEndpoint);
add_openid_provider([], _) ->
    ok.





start_web_interface() ->
    oidcc_client:register(tts_oidc_client),
    EpMain = ?CONFIG(ep_main),
    EpOidc = relative_path("oidc"),
    EpApiBase = relative_path("api"),
    EpStatic = relative_path("static/[...]"),
    EpApi = tts_rest:dispatch_mapping(EpApiBase),
    Dispatch = cowboy_router:compile( [{'_',
                                         [
                                          {EpStatic, cowboy_static,
                                           {priv_dir, tts, "http_static"}},
                                          {EpApi, tts_rest, []},
                                          %% {EpMain, tts_http_prep, []},
                                          {EpMain, cowboy_static,
                                           {priv_file, tts,
                                            "http_static/index.html"}},
                                          {EpOidc, oidcc_http_handler, []}
                                         ]}]),

    SSL = ?CONFIG(ssl),
    ListenPort = ?CONFIG(listen_port),

    case SSL of
        false ->
            {ok, _} = cowboy:start_http( http_handler
                                         , 100
                                         , [ {port, ListenPort} ]
                                         , [{env, [{dispatch, Dispatch}]}]
                                       );
        _ ->
            %% CaCertFile = ?CONFIG(ca_cert_file),
            CertFile = ?CONFIG(cert_file),
            KeyFile = ?CONFIG(key_file),
            {ok, _} = cowboy:start_https( http_handler
                                          , 100
                                          , [ {port, ListenPort},
                                              %% {cacertfile, CaCertFile},
                                              {certfile, CertFile},
                                              {keyfile, KeyFile}
                                            ]
                                          , [{env, [{dispatch, Dispatch}]}]
                                        )
    end,
    ok.

relative_path(Append) ->
    Base = ?CONFIG(ep_main),
    binary:list_to_bin(io_lib:format("~s~s", [Base, Append])).

local_endpoint() ->
    HostName = binary:list_to_bin(?CONFIG(hostname)),
    EpReturn = <<"/oidc">>,
    Prot = local_protocol(),
    Port = local_port(),
    << Prot/binary, HostName/binary, Port/binary, EpReturn/binary >>.


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
