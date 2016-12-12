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
    gen_server:cast(self(), start_database),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(start_database, State) ->
    start_database(),
    gen_server:cast(self(), add_oidc),
    {noreply, State};
handle_cast(add_oidc, State) ->
    add_openid_provider(),
    gen_server:cast(self(), add_services),
    {noreply, State};
handle_cast(add_services, State) ->
    add_services(),
    gen_server:cast(self(), start_http),
    {noreply, State};
handle_cast(start_http, State) ->
    start_web_interface(),
    stop(self()),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



start_database() ->
    ok = tts_data_sqlite:reconfigure(),
    ok.


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


add_services() ->
    ServiceList = ?CONFIG(service_list),
    ok = add_services(ServiceList),
    ok.

add_services([ConfigMap | T]) ->
    {ok, Id} = tts_service:add(ConfigMap),
    tts_service:update_params(Id),
    add_services(T);
add_services(undefined) ->
    ok;
add_services([]) ->
    ok.



start_web_interface() ->
    oidcc_client:register(tts_oidc_client),
    EpMain = ?CONFIG(ep_main),
    EpOidc = tts_http_util:relative_path("oidc"),
    EpApiBase = tts_http_util:relative_path("api"),
    EpStatic = tts_http_util:relative_path("static/[...]"),
    EpApi = tts_rest:dispatch_mapping(EpApiBase),
    Dispatch = cowboy_router:compile( [{'_',
                                         [
                                          {EpStatic, cowboy_static,
                                           {priv_dir, tts, "http_static"}},
                                          {EpApi, tts_rest, []},
                                          {EpMain, cowboy_static,
                                           {priv_file, tts,
                                            "http_static/index.html"}},
                                          {EpOidc, oidcc_cowboy, []}
                                         ]}]),

    SSL = ?CONFIG(ssl),
    ListenPort = ?CONFIG(listen_port),
    case SSL of
        true ->
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
                                        );
        false ->
            {ok, _} = cowboy:start_http( http_handler
                                         , 100
                                         , [ {port, ListenPort} ]
                                         , [{env, [{dispatch, Dispatch}]}]
                                       )
    end,
    Redirect = ?CONFIG(redirection_enable),
    RedirectPort = ?CONFIG(redirection_port),
    RedirDispatch = cowboy_router:compile([{'_',
                                                [
                                                 {"/[...]", tts_redirection, []}
                                                 ]}]),
    case Redirect of
        true ->
            {ok, _} = cowboy:start_http( redirect_handler
                                          , 100
                                          , [ {port, RedirectPort}]
                                          , [{env, [{dispatch, RedirDispatch}]}]
                                        );
        _ -> ok
    end,

    ok.


local_endpoint() ->
    tts_http_util:whole_url(tts_http_util:relative_path("oidc")).
