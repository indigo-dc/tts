-module(watts_init).
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

-include("watts.hrl").

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
    gen_server:cast(self(), start_watts),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(start_watts, State) ->
    init_watts(),
    gen_server:cast(self(), start_database),
    {noreply, State};
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
    stop(),
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


init_watts() ->
    lager:info("Init: starting  "),
    %% copy the version into the config
    %% only using env values, so everything can be tested
    Vsn =  case ?GETKEY(vsn) of
               undefined -> "testing";
               {ok, V} -> V
           end,
    ok = ?SETCONFIG(vsn, Vsn),
    lager:debug("Init: config = ~p", [?ALLCONFIG]),
    ok.


start_database() ->
    lager:info("Init: starting ets database"),
    ok = watts_data:init(),
    lager:info("Init: starting sqlite database ~p", [?CONFIG(sqlite_db)]),
    ok = watts_data_sqlite:reconfigure(),
    case watts_data_sqlite:is_ready() of
        ok -> ok;
        {error, Reason} ->
            Msg = io_lib:format("unable to start sqlite-db: ~p", [Reason]),
            lager:critical(Msg),
            erlang:error(no_database)
    end,
    ok.


add_openid_provider() ->
    lager:debug("Init: adding openid provider"),
    LocalEndpoint = local_endpoint(),
    ProviderList = ?CONFIG(provider_list, []),
    ok = add_openid_provider(ProviderList, LocalEndpoint),
    ok.

add_openid_provider([#{id := Id, description := Desc, client_id := ClientId,
                       client_secret := Secret, scopes := RequestScopes,
                       config_endpoint := ConfigEndpoint}|T], LocalEndpoint) ->
        lager:debug("Init: adding provider ~p", [Id]),
    try
        {ok, _InternalId, _Pid} =
        oidcc:add_openid_provider(Id, Id, Desc, ClientId, Secret,
                                  ConfigEndpoint, LocalEndpoint, RequestScopes),
        add_openid_provider(T, LocalEndpoint)
    catch Error:Reason ->
            lager:critical("error occured OpenId Connect provider ~p: '~p' ~p",
                           [Id, Error, Reason])
    end;
add_openid_provider([], _) ->
    ok.


add_services() ->
    lager:info("Init: adding services"),
    ServiceList = ?CONFIG(service_list),
    ok = add_services(ServiceList),
    ok.

add_services([#{id := Id } = ConfigMap | T]) ->
    lager:debug("Init: adding service ~p", [Id]),
    try
        {ok, Id} = watts_service:add(ConfigMap),
        ok = watts_service:update_params(Id)
     catch Error:Reason ->
            lager:critical("error occured during adding service ~p: '~p' ~p",
                           [Id, Error, Reason])
     end,
     add_services(T);
add_services(undefined) ->
    ok;
add_services([]) ->
    ok.



start_web_interface() ->
    lager:info("Init: starting web interface"),
    oidcc_client:register(watts_oidc_client),
    EpMain = ?CONFIG(ep_main),
    EpOidc = watts_http_util:relative_path("oidc"),
    EpApiBase = watts_http_util:relative_path("api"),
    EpStatic = watts_http_util:relative_path("static/[...]"),
    EpApi = watts_rest:dispatch_mapping(EpApiBase),
    Dispatch = cowboy_router:compile(
                 [{'_', [{EpStatic, cowboy_static,
                          {priv_dir, ?APPLICATION, "http_static"}
                         },
                         {EpApi, watts_rest, []},
                         {EpMain, cowboy_static,
                          {priv_file, ?APPLICATION, "http_static/index.html"}},
                         {EpOidc, oidcc_cowboy, []}
                        ]
                  }]
                ),
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
    RedirDispatch = cowboy_router:compile(
                      [{'_', [
                              {"/[...]", watts_redirection, []}
                             ]
                       }]
                     ),
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
    watts_http_util:whole_url(watts_http_util:relative_path("oidc")).


stop() ->
    lager:info("Init: done"),
    lager:info("WaTTS ready"),
    stop(self()).
