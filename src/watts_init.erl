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
%% @doc The {@module} module takes care of the initialization of WaTTS.
%% It uses the application environment set by the cuttlefish.
%%
%% Cuttlefish is a config parsing and validation tool creating a set of
%% parameter for the actual application to run, it is configured by schemas,
%% those are stored at /config/schema.
%%
%% Once the configuration file is parsed and is valid the result is passed to
%% the application environment of WaTTS.
%%
%% Once WaTTS and all it's dependencies are loaded this part starts running and
%% configures the application.
%%
%% The steps taken to configure the server include:
%% <ul>
%% <li> setup the database </li>
%% <li> initilize the OpenID Connect provider </li>
%% <li> configure the services with their plugins </li>
%% <li> start the web interface </li>
%% </ul>
%% The most important functions are {@link init/1} and {@link handle_cast/2}.
-module(watts_init).



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
%% @doc starting the gen_server process.
start_link() ->
    gen_server:start_link(?MODULE, no_parameter, []).

-spec stop(pid()) -> ok.
%% @doc function to stop the process.
stop(Pid) ->
    gen_server:cast(Pid, stop).


%% gen_server.
%% @doc staring the initializing process.
%% The function just sends a cast to itself which in turn will be
%% handled by handle_cast.
%% @see handle_cast/2
-spec init(no_parameter) -> {ok, tuple()}.
init(no_parameter) ->
    gen_server:cast(self(), check_watts_not_started),
    {ok, #state{}}.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec handle_call(any(), any(), tuple()) -> {reply, ignored, tuple()}.
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%% @doc this is where the magic happens.
%% The process handles one step at a time and after each step it will
%% send a cast to itself to trigger the next step.
%%
%% The folowing steps are processed (in order of excution):
%% <ul>
%% <li> ensure the init process has not been started before </li>
%% <li> set the version and ensure not running as root <em>init_watts</em> </li>
%% <li> starting the database <em>start_database</em> </li>
%% <li> adding the OpenID Connect provider <em>add_openid_provider</em> </li>
%% <li> maybe adding RSPs, if configured <em>maybe_add_rsps</em> </li>
%% <li> add the configured services <em>add_services</em> </li>
%% <li> start the web interface <em>start_web_interface</em></li>
%% </ul>
%% @see start_if_not_starte_before/0
%% @see init_watts/0
%% @see start_database/0
%% @see add_openid_provider/0
%% @see maybe_add_rsps/1
%% @see add_services/0
%% @see start_web_interface/0
-spec handle_cast(any(), tuple()) -> {noreply, tuple()} |
                                      {stop, normal, tuple()}.
handle_cast(check_watts_not_started, State) ->
    start_if_not_started_before(),
    {noreply, State};
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
    gen_server:cast(self(), add_rsps),
    {noreply, State};
handle_cast(add_rsps, State) ->
    maybe_add_rsps(?CONFIG(enable_rsp)),
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
handle_cast(stop, #state{} = State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec handle_info(any(), tuple()) -> {noreply, tuple()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec terminate(any(), tuple()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec code_change(any(), tuple(), any()) -> {ok, tuple()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc start the configuration of WaTTS if it has not been started before.
%% If it has been started before, the configuration crashed, which means
%% that something unexpected happened. As this is a critical issue WaTTS
%% will be stoppped then.
-spec start_if_not_started_before() -> ok.
start_if_not_started_before() ->
    start_if_undefined(?CONFIG(watts_init_started)).

%% @doc only start if the special configuration `watts_init_started' is not set.
-spec start_if_undefined(Started :: undefined | any()) -> ok.
start_if_undefined(undefined) ->
    ok = ?SETCONFIG(watts_init_started, true),
    gen_server:cast(self(), start_watts);
start_if_undefined(_) ->
    lager:emergency("Init: restarting ... this should not happen!"),
    erlang:halt(255).

%% @doc start initalization of WaTTS.
%% This copies the version from keys to environment and enforces security
%% @see enforce_security/0
-spec init_watts() -> ok.
init_watts() ->
    lager:info("Init: starting  "),
    %% copy the version into the config
    %% only using env values, so everything can be tested
    Vsn =  case ?GETKEY(vsn) of
               undefined -> "testing";
               {ok, V} -> V
           end,
    ok = ?SETCONFIG(vsn, Vsn),
    lager:info("Init: WaTTS version ~p", [Vsn]),
    ok = enforce_security(),
    lager:debug("Init: config = ~p", [?ALLCONFIG]),
    ok.

%% @doc enforces WaTTS to run in a secure setting.
%% This includes running as non root user and having SSL configured.
%% If SSL is not configured it will be forced to localhost.
%% @see maybe_change_hostname/3
%% @see error_if_running_as_root/0
-spec enforce_security() -> ok.
enforce_security() ->
    SSL = ?CONFIG(ssl),
    Hostname0 = ?CONFIG(hostname),
    Onion = lists:suffix(".onion", Hostname0),
    Hostname = maybe_change_hostname(SSL, Onion, Hostname0),
    ?SETCONFIG(hostname, Hostname),
    ok = error_if_running_as_root(),
    ok.

%% @doc change the hostname to localhost if not configured well.
%% It will change to localhost if neither configured to run as a
%% tor hidden service, nor having SSL configured.
-spec maybe_change_hostname(HasSSL :: boolean(), IsOnion :: boolean(),
                            Hostname :: list()) -> NewHostname :: list().
maybe_change_hostname(false, false, _) ->
    H = "localhost",
    lager:warning("Init: Neither SSL nor Tor is configured; "
                  "hostname set to ~p", [H]),
    H;
maybe_change_hostname(_, _, Hostname) ->
    Hostname.

%% @doc ensure WaTTS is not running as root.
%% if it is running as root the VM gets stopped.
%% @see maybe_root_halt/2
-spec error_if_running_as_root() -> ok.
error_if_running_as_root() ->
    Uid = list_to_integer(remove_newline(os:cmd("id -u"))),
    User = remove_newline(os:cmd("id -un")),
    ok = maybe_root_halt(User, Uid).

%% @doc halt the VM if uid is 0 or user is 'root'.
-spec maybe_root_halt(User :: list(), Uid :: number()) -> ok.
maybe_root_halt(User, Uid) when User == "root"; Uid == 0 ->
    lager:critical("Init: do not run WaTTS as root, stopping"),
    erlang:halt(1);
maybe_root_halt(User, Uid) ->
    lager:info("Init: running as user ~p [~p]", [User, Uid]),
    ok.

%% @doc start the databases needed to run WaTTS.
%% The in ram database is started using watts_ets and the
%% configured persistent database is started with watts_persistent.
%% @see watts_ets:init/0
%% @see watts_persistent:init/0
-spec start_database() -> ok.
start_database() ->
    lager:info("Init: starting ram database"),
    ok = watts_ets:init(),
    lager:info("Init: starting persistence database"),
    ok = watts_persistent:init(),
    case watts_persistent:is_ready() of
        true -> ok;
        {false, R} ->
            Msg = io_lib:format("unable to start persistence layer: ~p", [R]),
            lager:critical(Msg),
            erlang:error(no_database)
    end,
    ok.

%% @doc add the configured openid provider.
%% this function iterates throught the configured providers and
%% adds each of them. Then waits for the results so the provider can
%% read the needed configs from the Internet in parallel.
%% @see add_openid_provider/2
%% @see wait_and_log_provider_results/0
-spec add_openid_provider() -> ok.
add_openid_provider() ->
    lager:info("Init: adding openid provider"),
    %% force only one try
    application:set_env(oidcc, provider_max_tries, 1),
    LocalEndpoint = local_endpoint(),
    ?SETCONFIG(local_endpoint, LocalEndpoint),
    lager:info("Init: using local endpoint ~p", [LocalEndpoint]),
    ProviderList = ?CONFIG(provider_list, []),
    ok = add_openid_provider(ProviderList, LocalEndpoint),
    ok = wait_and_log_provider_results(),
    ok.

%% @doc take one provider from the list and add it.
%% This uses the oidcc library to handle the OpenID Connect provider.
-spec add_openid_provider(Configs:: [map()], LocalEndpoint :: binary()) -> ok.
add_openid_provider([#{id := Id,
                       config_endpoint := ConfigEndpoint,
                       disable_login := Disable} = Config0 | T],
                    LocalEndpoint) ->
    try
        Priority = length(T),
        Config = maps:put(priority, Priority,
                          maps:remove(config_endpoint, Config0)),
        {ok, _InternalId, _Pid} =
        oidcc:add_openid_provider(ConfigEndpoint, LocalEndpoint, Config),
        lager:info("Init: added OpenId Connect provider ~p", [Id]),
        case Disable of
            true ->
                lager:info("Init: ~p will not be used for login", [Id]);
            false ->
                ok
        end
    catch Error:Reason ->
            Msg = "Init: error occured OpenId Connect provider ~p: '~p' ~p",
            lager:critical(Msg, [Id, Error, Reason])
    end,
    add_openid_provider(T, LocalEndpoint);
add_openid_provider([], _) ->
    ok.


%% @doc waits for the configured provides to either fail or be ready.
%% The configuration max_provider_wait sets the max time to wait, the
%% default is 5 seconds. Reducing this time will speedup the startup.
%% @see wait_and_log_provider_results/3
-spec wait_and_log_provider_results() -> ok.
wait_and_log_provider_results() ->
    {ok, List} = oidcc:get_openid_provider_list(),
    Max = erlang:system_time(seconds) + ?CONFIG(max_provider_wait, 5),
    ok = wait_and_log_provider_results(List, [], Max),
    ok.

%% @doc iterate through the list of oidc provider and check their status.
%% A new list is set up with pending provider and maybe checked again if
%% the timeout has not yet been reached.
%% @see maybe_recheck_provider/3
-spec wait_and_log_provider_results(Provider :: [{Id::binary(), Pid::pid()}],
                                    Pending :: [{Id::binary(), Pid::pid()}],
                                    Timeout :: integer()) -> ok.
wait_and_log_provider_results([], [], _Max) ->
    ok;
wait_and_log_provider_results([], List, Max) ->
    InTime = (erlang:system_time(seconds) < Max),
    maybe_recheck_provider(InTime, List, Max);
wait_and_log_provider_results([{Id, Pid} = H | T], List, Max) ->
    {ok, #{ready := Ready}} = oidcc_openid_provider:get_config(Pid),
    {ok, Error} = oidcc_openid_provider:get_error(Pid),
    IsError = (Error /= undefined),
    NewList =
        case {Ready, IsError} of
            {true, _} ->
                lager:info("Init: OpenId Connect provider ~p ready", [Id]),
                List;
            {_, true} ->
                lager:warning("Init: OpenId Connect provider ~p has error ~p",
                              [Id, Error]),
                List;
            _ ->
                [ H | List ]
        end,
    wait_and_log_provider_results(T, NewList, Max).

%% @doc recheck the provider for their status if still in time.
-spec maybe_recheck_provider(InTime :: boolean(),
                             ProviderPending :: [{Id::binary(), Pid::pid()}],
                             MaxTime :: integer()) -> ok.
maybe_recheck_provider(true, List, Max) ->
    timer:sleep(200),
    wait_and_log_provider_results(List, [], Max);
maybe_recheck_provider(false, List, _) ->
    Output =
        fun({Id, _Pid}, _) ->
                lager:info("Init: OpenId Connect provider ~p takes too long, "
                           "won't wait for its result", [Id])
        end,
    lists:foldl(Output, ok, List),
    ok.


%% @doc Add RSPs if enabled in the configuration.
%% Relaying Service Provider are only added if configured, else the
%% configured RSPs are not added to the running WaTTS instance.
%% @see add_rsps/0
-spec maybe_add_rsps(Enabled :: boolean() ) -> ok.
maybe_add_rsps(true) ->
    add_rsps();
maybe_add_rsps(_) ->
    ?SETCONFIG(rsp_list, []),
    ok.

%% @doc Add the configured RSPs as they are enabled in the config.
%% The function iterates through the configuration and updates them.
%% No keys are fetched yet, only the configuration is updated.
%% @see watts_rsp:new/1
%% @see watts_rsp
-spec add_rsps() -> ok.
add_rsps() ->
    lager:info("Init: adding relying service provider (RSP)"),
    UpdateRsp =
        fun(#{id := Id} = Config, List) ->
                {ok, Rsp} = watts_rsp:new(Config),
                lager:info("Init: added RSP ~p (keys not yet fetched)", [Id]),
                [ Rsp | List ]
        end,
    NewRspList = lists:foldl(UpdateRsp, [], ?CONFIG(rsp_list, [])),
    ?SETCONFIG(rsp_list, NewRspList),
    ok.


%% @doc add the services to the server and log the results.
%% @see watts_service:add/1
%% @see watts_service:update_params/1
-spec add_services() -> ok.
add_services() ->
    AddService =
        fun(#{id := Id} = ConfigMap, _) ->
                lager:debug("Init: adding service ~p", [Id]),
                try
                    {ok, Id} = watts_service:add(ConfigMap),
                    ok = watts_service:update_params(Id)
                catch Error:Reason ->
                     Msg = "error occured during adding service ~p: '~p' ~p",
                     lager:critical(Msg, [Id, Error, Reason])
                end
        end,

    lager:info("Init: adding services"),
    ServiceList = ?CONFIG(service_list, []),
    lists:foldl(AddService, ok, ServiceList),
    ok.


%% @doc start the web interface of WaTTS.
%% This starts the main web server with the API, the static file serving of
%% the java script SPA and, if configured, the documentations. The redirection
%% from http to https endpoint is also started if configured.
%%
%% To configure SSL first the files are read and if that fails SSL is disabled.
%% @see create_dispatch_list/0
%% @see add_options/5
-spec start_web_interface() -> ok.
start_web_interface() ->
    lager:info("Init: starting web interface"),
    oidcc_client:register(watts_oidc_client),
    Dispatch = cowboy_router:compile([{'_', create_dispatch_list()}]),
    SSL = ?CONFIG(ssl),
    UseSSL = read_ssl_files(SSL),
    ListenPort = ?CONFIG(listen_port),
    case UseSSL of
        true ->
            Cert = ?CONFIG(cert),
            Key = ?CONFIG(key),
            BasicOptions =
                [ {port, ListenPort},
                  {cert, Cert},
                  {key, Key}
                ],
            IsOnion = lists:suffix(".onion", ?CONFIG(hostname)),
            Options = add_options(BasicOptions, ?CONFIG_(cachain),
                                  ?CONFIG_(dhparam), IsOnion,
                                  ?CONFIG_(enable_ipv6)),
            {ok, _} = cowboy:start_https( http_handler
                                          , ?CONFIG(num_acceptors)
                                          , Options
                                          , [{env, [{dispatch, Dispatch}]}]
                                        );
        false ->
            LocalIp =
                case ?CONFIG(enable_ipv6) of
                    false ->
                        lager:warning("Init: listening only at 127.0.0.1"),
                        {127, 0, 0, 1};
                    true ->
                        lager:warning("Init: listening only at ::1"),
                        {0, 0, 0, 0, 0, 0, 0, 1}
                end,

            {ok, _} = cowboy:start_http( http_handler
                                         , ?CONFIG(num_acceptors)
                                         , [ {port, ListenPort},
                                             {ip, LocalIp}
                                             ]
                                         , [{env, [{dispatch, Dispatch}]}]
                                       )
    end,
    Redirect = ?CONFIG(redirection_enable),
    RedirectPort = ?CONFIG(redirection_port),
    RedirDispatch = cowboy_router:compile(
                      [{'_', [
                              {"/[...]", watts_http_redirect, []}
                             ]
                       }]
                     ),
    case Redirect and SSL of
        true ->
            lager:info("Init: enable redirection at port ~p", [RedirectPort]),
            IPv6 = case ?CONFIG(enable_ipv6) of
                       true ->  [inet6];
                       false -> []
                   end,
            {ok, _} = cowboy:start_http( redirect_handler
                                       , 10
                                       , [ {port, RedirectPort} | IPv6  ]
                                       , [{env, [{dispatch, RedirDispatch}]}]
                                       );
        _ -> ok
    end,

    ok.


%% @doc try to read the SSL files and return if successful.
%% This function is reading
%% <ul>
%% <li> the certificat </li>
%% <li> the private key </li>
%% <li> the ca chain </li>
%% <li> the dh params </li>
%% </ul>
%% SSL gets disable if one of them fails.
-spec read_ssl_files(ShouldBeRead :: boolean()) -> UseSSL :: boolean().
read_ssl_files(true) ->
    CertOK = read_certificate(?CONFIG_(cert_file)),
    KeyOK = read_key(?CONFIG_(key_file)),
    ChainOK = read_cachain(?CONFIG_(cachain_file)),
    DhOK = read_dhparam(?CONFIG_(dh_file)),
    CertOK and KeyOK and ChainOK and DhOK;
read_ssl_files(_) ->
    false.

%% @doc read the certificate for SSL
-spec read_certificate(any()) -> Success :: boolean().
read_certificate({ok, Path}) ->
    case read_pem_entries(Path) of
        [{'Certificate', Certificate, not_encrypted}] ->
            ?SETCONFIG(cert, Certificate),
            true;
        _ ->
            lager:error("Init: certificate ~p invalid", [Path]),
            false
    end;
read_certificate(_) ->
    false.

%% @doc read the private key for SSL
-spec read_key(any()) -> Success :: boolean().
read_key({ok, Path}) ->
    case read_pem_entries(Path) of
        [{Type, PrivateKey, not_encrypted}]
        when Type == 'RSAPrivateKey'; Type == 'DSAPrivateKey';
             Type =='ECPrivateKey'; Type == 'PrivateKeyInfo'->
            ?SETCONFIG(key, {Type, PrivateKey}),
            true;
        _ ->
            lager:error("Init: private key ~p invalid", [Path]),
            false
    end;
read_key(_) ->
    false.

%% @doc read the ca chain for SSL
-spec read_cachain(any()) -> Success :: boolean().
read_cachain({ok, Path}) ->
    case read_pem_entries(Path) of
        [] ->
            lager:error("Init: ca chain ~p is empty", [Path]),
            ?UNSETCONFIG(cachain),
            false;
        PemCerts ->
            Decode = fun({'Certificate', Cert, not_encrypted}, List) ->
                             [ Cert | List ];
                        (_, List) ->
                             List
                     end,
            Certs = lists:foldl(Decode, [], PemCerts),
            ?SETCONFIG(cachain, Certs),
            true
    end;
read_cachain(_) ->
    lager:warning("Init: no ca-chain-file configured [cachain_file]!"),
    ?UNSETCONFIG(cachain),
    true.

%% @doc read the dh params for SSL
-spec read_dhparam(any()) -> Success :: boolean().
read_dhparam({ok, none}) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    ?UNSETCONFIG(dhparam),
    true;
read_dhparam({ok, Path}) ->
    case read_pem_entries(Path) of
        [{'DHParameter', DhParam, not_encrypted}] ->
            ?SETCONFIG(dhparam, DhParam),
            true;
        _ ->
            lager:error("Init: dh-file ~p is invalid", [Path]),
            ?UNSETCONFIG(dhparam),
            false
    end;
read_dhparam(_) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    ?UNSETCONFIG(dhparam),
    true.

%% @doc helper function to read pem encoded files
-spec read_pem_entries(Path :: binary()) -> [tuple()].
read_pem_entries(Path) ->
    extract_pem(file:read_file(Path), Path).

%% @doc helper function to decode pem entries
-spec extract_pem({ok, binary()} | any(), binary()) -> [tuple()].
extract_pem({ok, PemBin}, _) ->
    public_key:pem_decode(PemBin);
extract_pem(Error, Path) ->
    lager:error("Init: error reading file ~p: ~p", [Path, Error]),
    [].


%% @doc create the dispatch list for the web interface.
%% This is the list of endpoints and the corresponding action to happen,
%% this could be either calling a function or serving static files.
%%
%% The function creates a basic dispatch list for the javascript to be served,
%% the OpenID connect handling (login) and the api.
%% Then calls the create_dispatch_list/2 function to configure the dynamic part.
%% @see create_dispatch_list/2
-spec create_dispatch_list() -> [tuple()].
create_dispatch_list() ->
    EpMain = ?CONFIG(ep_main),
    EpOidc = watts_http_util:relative_path("oidc"),
    EpStatic = watts_http_util:relative_path("static/[...]"),
    EpApi = watts_http_util:relative_path("api/[...]"),
    BaseDispatchList = [{EpStatic, cowboy_static,
                         {priv_dir, ?APPLICATION, "http_static"}
                        },
                        {EpApi, watts_http_api, []},
                        {EpMain, cowboy_static,
                         {priv_file, ?APPLICATION, "http_static/index.html"}},
                        {EpOidc, oidcc_cowboy, []}
                       ],
    create_dispatch_list([{doc_user, ?CONFIG(enable_user_doc)},
                          {doc_code, ?CONFIG(enable_code_doc)},
                          {rsp, ?CONFIG(enable_rsp), ?CONFIG(rsp_list)},
                          {privacy, ?CONFIG(privacy_doc)} ],
                         BaseDispatchList).

%% @doc handle each configuration and transform it into a dispatch entry.
%% The handled settings include
%% <ul>
%% <li> user documentation </li>
%% <li> code documentation </li>
%% <li> rps endpoint </li>
%% <li> privacy statement </li>
%% </ul>
-spec create_dispatch_list(Config:: [tuple()], DispatchList :: [tuple()])
                          -> [tuple()].
create_dispatch_list([], List) ->
     List;
create_dispatch_list([{doc_user, true} | T], List) ->
    DocInfo = "Init: publishing user documentation at /docs/user/",
    lager:info(DocInfo),
    EpDocs = watts_http_util:relative_path("docs/user/[...]"),
    NewList = [ {EpDocs, cowboy_static, {priv_dir, ?APPLICATION, "docs/user"}}
                | List ],
    create_dispatch_list(T, NewList);
create_dispatch_list([{doc_code, true} | T], List) ->
    DocInfo = "Init: publishing code documentation at /docs/code/",
    lager:info(DocInfo),
    EpDocs = watts_http_util:relative_path("docs/code/[...]"),
    NewList = [ {EpDocs, cowboy_static, {priv_dir, ?APPLICATION, "docs/code"}}
                | List ],
    create_dispatch_list(T, NewList);
create_dispatch_list([{rsp, true, []} | T], List) ->
    lager:info("Init: relying service provider won't be enabled as none is "
               "configured"),
    create_dispatch_list(T, List);
create_dispatch_list([{rsp, true, _} | T], List) ->
    RspInfo = "Init: enable relying service provider at /rsp/",
    lager:info(RspInfo),
    EpRsp = watts_http_util:relative_path("rsp/[...]"),
    NewList = [ {EpRsp, watts_http_rsp, []} | List ],
    create_dispatch_list(T, NewList);
create_dispatch_list([{privacy, undefined} | T], List) ->
    EpPrivacy = watts_http_util:relative_path("privacystatement.html"),
    PrivWarn = "Init: The privacy statement is not configured [~p]",
    lager:warning(PrivWarn, [privacy_doc]),
    NewList = [ { EpPrivacy , cowboy_static,
                  {priv_file, ?APPLICATION, "no_privacy.html"}
                }
                | List],
    create_dispatch_list(T, NewList);
create_dispatch_list([{privacy, File} | T], List) ->
    EpPrivacy = watts_http_util:relative_path("privacystatement.html"),
    PrivInfo = "Init: Using privacy statement ~p",
    lager:info(PrivInfo, [File]),
    NewList = [ {EpPrivacy, cowboy_static, {file, File}} | List],
    create_dispatch_list(T, NewList);
create_dispatch_list([_ | T], List) ->
    create_dispatch_list(T, List).



%% @doc dynamically generate the list of options for the webserver.
-spec add_options(Options :: [tuple()], CaChainSetting :: tuple() | ok,
                  DhParamSetting :: tuple() | ok, IsOnion :: boolean() | ok,
                  IPv6Setting :: tuple() | ok) -> [tuple()].
add_options(Options, {ok, CaChain}, DhParam, IsOnion, IPv6) ->
    NewOptions = [ {cacerts, CaChain} | Options ],
    add_options(NewOptions, ok, DhParam, IsOnion, IPv6);
add_options(Options, undefined, DhParam, IsOnion, IPv6) ->
    add_options(Options, ok, DhParam, IsOnion, IPv6);
add_options(Options, CaChain,  undefined, IsOnion, IPv6) ->
    add_options(Options, CaChain, ok, IsOnion, IPv6);
add_options(Options, CaChain, {ok, DhParam}, IsOnion, IPv6) ->
    NewOptions = [ {dh, DhParam} | Options ],
    add_options(NewOptions, CaChain, ok, IsOnion, IPv6);
add_options(Options, CaChain, DhParam, true, {ok, true}) ->
    lager:info("Init: listening only at ::1 (onion)"),
    NewOptions = [ {ip, {0, 0, 0, 0, 0, 0, 0, 1}} | Options ],
    add_options(NewOptions, CaChain, DhParam, ok, {ok, true});
add_options(Options, CaChain, DhParam, true, IPv6) ->
    lager:info("Init: listening only at 127.0.0.1 (onion)"),
    NewOptions = [ {ip, {127, 0, 0, 1}} | Options ],
    add_options(NewOptions, CaChain, DhParam, ok, IPv6);
add_options(Options, CaChain, DhParam, IsOnion, {ok, true}) ->
    NewOptions =  [ inet6 | Options],
    add_options(NewOptions, CaChain, DhParam, IsOnion, ok);
add_options(Options, CaChain, DhParam, IsOnion, {ok, false}) ->
    NewOptions =  [ inet | Options],
    add_options(NewOptions, CaChain, DhParam, IsOnion, ok);
add_options(Options, _, _, _, _) ->
    Options.




%% @doc return the local endpoint
-spec local_endpoint() -> binary().
local_endpoint() ->
    watts_http_util:whole_url(watts_http_util:relative_path("oidc")).

%% @doc gracefully stop the configuration process when done.
-spec stop() -> ok.
stop() ->
    lager:info("Init: done"),
    lager:info("Init: startup took ~p seconds",
               [erlang:system_time(seconds) - ?CONFIG(start_time)]),
    lager:info("WaTTS ready"),
    stop(self()).

%% @doc helperfunction to remove newlines from data
-spec remove_newline(string()) -> string().
remove_newline(List) ->
    Filter = fun($\n) ->
                     false;
                (_) ->
                     true
             end,
    lists:filter(Filter, List).
