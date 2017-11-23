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

%% for mails
-export([error/1]).
-export([error/2]).
-export([warning/1]).
-export([warning/2]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          issues = [] :: [string()] | []
         }).
-type state() :: #state{}.

%% API.

-spec start_link() -> {ok, pid()}.
%% @doc starting the gen_server process.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_parameter, []).

-spec stop(pid()) -> ok.
%% @doc function to stop the process.
stop(Pid) ->
    gen_server:cast(Pid, stop).


error(Message) ->
    gen_server:cast(?MODULE, {log_error, Message}).

error(Message, Params) ->
    gen_server:cast(?MODULE, {log_error, Message, Params}).

warning(Message) ->
    gen_server:cast(?MODULE, {log_warning, Message}).

warning(Message, Params) ->
    gen_server:cast(?MODULE, {log_warning, Message, Params}).

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
-spec handle_cast(any(), state()) -> {noreply, tuple()} |
                                      {stop, normal, tuple()}.
handle_cast(check_watts_not_started, State) ->
    NewState = start_if_not_started_before(State),
    {noreply, NewState};
handle_cast(start_watts, State) ->
    NewState = init_watts(State),
    gen_server:cast(self(), start_database),
    {noreply, NewState};
handle_cast(start_database, State) ->
    NewState = start_database(State),
    gen_server:cast(self(), add_oidc),
    {noreply, NewState};
handle_cast(add_oidc, State) ->
    NewState = add_openid_provider(State),
    gen_server:cast(self(), add_rsps),
    {noreply, NewState};
handle_cast(add_rsps, State) ->
    NewState = maybe_add_rsps(?CONFIG(enable_rsp), State),
    gen_server:cast(self(), add_services),
    {noreply, NewState};
handle_cast(add_services, State) ->
    NewState = add_services(State),
    gen_server:cast(self(), update_jwt_keys),
    {noreply, NewState};
handle_cast(update_jwt_keys, State) ->
    NewState = update_jwt_keys(State),
    gen_server:cast(self(), start_http),
    {noreply, NewState};
handle_cast(start_http, State) ->
    State1 = start_web_interface(State),
    NewState = do_stop(State1),
    {noreply, NewState};
handle_cast(stop, #state{} = State) ->
    {stop, normal, State};
handle_cast({log_error, Message}, State) ->
    NewState = log_error(Message, State),
    {noreply, NewState};
handle_cast({log_error, Message, Params}, State) ->
    NewState = log_error(Message, Params, State),
    {noreply, NewState};
handle_cast({log_warning, Message}, State) ->
    NewState = log_warning(Message, State),
    {noreply, NewState};
handle_cast({log_warning, Message, Params}, State) ->
    NewState = log_warning(Message, Params, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec handle_info(any(), tuple()) -> {noreply, tuple()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec terminate(any(), tuple()) -> ok.
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    system_halt("Init: terminated with reason ~p", [Reason], 254).


%% @doc just a dummy to be compliant with the behaviour, no functionality.
-spec code_change(any(), tuple(), any()) -> {ok, tuple()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc start the configuration of WaTTS if it has not been started before.
%% If it has been started before, the configuration crashed, which means
%% that something unexpected happened. As this is a critical issue WaTTS
%% will be stoppped then.
-spec start_if_not_started_before(state()) -> state().
start_if_not_started_before(State) ->
    start_if_undefined(?CONFIG(watts_init_started)),
    State.

%% @doc only start if the special configuration `watts_init_started' is not set.
-spec start_if_undefined(Started :: undefined | any()) -> ok.
start_if_undefined(undefined) ->
    ok = ?SETCONFIG(watts_init_started, true),
    gen_server:cast(self(), start_watts);
start_if_undefined(_) ->
    system_halt("Init: restarting ... this should never happen!", [], 255).

%% @doc start initalization of WaTTS.
%% This copies the version from keys to environment and enforces security
%% @see enforce_security/0
-spec init_watts(state()) -> state().
init_watts(State) ->
    lager:info("Init: starting  "),
    %% copy the version into the config
    %% only using env values, so everything can be tested
    Vsn =  case ?GETKEY(vsn) of
               undefined -> "testing";
               {ok, V} -> V
           end,
    ok = ?SETCONFIG(vsn, Vsn),
    lager:info("Init: WaTTS version ~p", [Vsn]),
    NewState = enforce_security(State),
    lager:debug("Init: config = ~p", [?ALLCONFIG]),
    NewState.

%% @doc enforces WaTTS to run in a secure setting.
%% This includes running as non root user and having SSL configured.
%% If SSL is not configured it will be forced to localhost.
%% @see maybe_change_hostname/3
%% @see error_if_running_as_root/0
-spec enforce_security(state()) -> state().
enforce_security(State) ->
    SSL = ?CONFIG(ssl),
    Hostname0 = ?CONFIG(hostname),
    Onion = lists:suffix(".onion", Hostname0),
    {Hostname, NewState} = maybe_change_hostname(SSL, Onion, Hostname0, State),
    ?SETCONFIG(hostname, Hostname),
    ok = error_if_running_as_root(),
    NewState.

%% @doc change the hostname to localhost if not configured well.
%% It will change to localhost if neither configured to run as a
%% tor hidden service, nor having SSL configured.
-spec maybe_change_hostname(HasSSL :: boolean(), IsOnion :: boolean(),
                            Hostname :: list(), state())
                           -> {NewHostname :: list(), state()}.
maybe_change_hostname(false, false, _, State) ->
    H = "localhost",
    NewState = log_warning("Neither SSL nor Tor is configured; "
                  "hostname set to ~p", [H], State),
    {H, NewState};
maybe_change_hostname(_, _, Hostname, State) ->
    {Hostname, State}.

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
    system_halt("Init: do not run WaTTS as root, stopping", [], 1);
maybe_root_halt(User, Uid) ->
    lager:info("Init: running as user ~p [~p]", [User, Uid]),
    ok.

%% @doc start the databases needed to run WaTTS.
%% The in ram database is started using watts_ets and the
%% configured persistent database is started with watts_persistent.
%% @see watts_ets:init/0
%% @see watts_persistent:init/0
-spec start_database(state()) -> state().
start_database(State) ->
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
    State.

%% @doc add the configured openid provider.
%% this function iterates throught the configured providers and
%% adds each of them. Then waits for the results so the provider can
%% read the needed configs from the Internet in parallel.
%% @see add_openid_provider/2
%% @see wait_and_log_provider_results/0
-spec add_openid_provider(state()) -> state().
add_openid_provider(State) ->
    lager:info("Init: adding openid provider"),
    %% force only one try
    application:set_env(oidcc, provider_max_tries, 1),
    LocalEndpoint = local_endpoint(),
    ?SETCONFIG(local_endpoint, LocalEndpoint),
    lager:info("Init: using local endpoint ~p", [LocalEndpoint]),
    ProviderList = ?CONFIG(provider_list, []),
    ok = add_openid_provider(ProviderList, LocalEndpoint),
    NewState = wait_and_log_provider_results(State),
    NewState.

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
-spec wait_and_log_provider_results(state()) -> state().
wait_and_log_provider_results(State) ->
    {ok, List} = oidcc:get_openid_provider_list(),
    Max = erlang:system_time(seconds) + ?CONFIG(max_provider_wait, 5),
    NewState = wait_and_log_provider_results(List, [], Max, State),
    NewState.

%% @doc iterate through the list of oidc provider and check their status.
%% A new list is set up with pending provider and maybe checked again if
%% the timeout has not yet been reached.
%% @see maybe_recheck_provider/3
-spec wait_and_log_provider_results(Provider :: [{Id::binary(), Pid::pid()}],
                                    Pending :: [{Id::binary(), Pid::pid()}],
                                    Timeout :: integer(), state()) -> state().
wait_and_log_provider_results([], [], _Max, State) ->
    State;
wait_and_log_provider_results([], List, Max, State) ->
    InTime = (erlang:system_time(seconds) < Max),
    maybe_recheck_provider(InTime, List, Max, State);
wait_and_log_provider_results([{Id, Pid} = H | T], List, Max, State) ->
    {ok, #{ready := Ready}} = oidcc_openid_provider:get_config(Pid),
    {ok, Error} = oidcc_openid_provider:get_error(Pid),
    IsError = (Error /= undefined),
    {NewList, NewState} =
        case {Ready, IsError} of
            {true, _} ->
                lager:info("Init: OpenId Connect provider ~p ready", [Id]),
                {List, State};
            {_, true} ->
                NewSt = log_warning("OpenId Connect provider ~p has error ~p",
                              [Id, Error], State),
                {List, NewSt};
            _ ->
                {[ H | List ], State}
        end,
    wait_and_log_provider_results(T, NewList, Max, NewState).

%% @doc recheck the provider for their status if still in time.
-spec maybe_recheck_provider(InTime :: boolean(),
                             ProviderPending :: [{Id::binary(), Pid::pid()}],
                             MaxTime :: integer(), state()) -> state().
maybe_recheck_provider(true, List, Max, State) ->
    timer:sleep(200),
    wait_and_log_provider_results(List, [], Max, State);
maybe_recheck_provider(false, List, _, State) ->
    Output =
        fun({Id, _Pid}, S) ->
                log_warning("OpenId Connect provider ~p takes too long, "
                           "won't wait for its result", [Id], S)
        end,
    NewState = lists:foldl(Output, State, List),
    NewState.


%% @doc Add RSPs if enabled in the configuration.
%% Relaying Service Provider are only added if configured, else the
%% configured RSPs are not added to the running WaTTS instance.
%% @see add_rsps/0
-spec maybe_add_rsps(Enabled :: boolean(), state() ) -> state().
maybe_add_rsps(true, State) ->
    add_rsps(State);
maybe_add_rsps(_, State) ->
    ?SETCONFIG(rsp_list, []),
    State.

%% @doc Add the configured RSPs as they are enabled in the config.
%% The function iterates through the configuration and updates them.
%% No keys are fetched yet, only the configuration is updated.
%% @see watts_rsp:new/1
%% @see watts_rsp
-spec add_rsps(state()) -> state().
add_rsps(State) ->
    lager:info("Init: adding relying service provider (RSP)"),
    UpdateRsp =
        fun(#{id := Id} = Config, List) ->
                {ok, Rsp} = watts_rsp:new(Config),
                lager:info("Init: added RSP ~p (keys not yet fetched)", [Id]),
                [ Rsp | List ]
        end,
    NewRspList = lists:foldl(UpdateRsp, [], ?CONFIG(rsp_list, [])),
    ?SETCONFIG(rsp_list, NewRspList),
    State.


%% @doc add the services to the server and log the results.
%% @see watts_service:add/1
%% @see watts_service:update_params/1
-spec add_services(state()) -> state().
add_services(State) ->
    AddService =
        fun(#{id := Id} = ConfigMap, S) ->
                lager:debug("Init: adding service ~p", [Id]),
                try
                    {ok, Id} = watts_service:add(ConfigMap),
                    ok = watts_service:update_params(Id),
                    S
                catch Error:Reason ->
                     Msg = "error occured during adding service ~p: '~p' ~p",
                     log_error(Msg, [Id, Error, Reason], S)
                end
        end,

    lager:info("Init: adding services"),
    ServiceList = ?CONFIG(service_list, []),
    NewState = lists:foldl(AddService, State, ServiceList),
    NewState.

%% @doc generate new signing keys for jwt
-spec update_jwt_keys(state()) -> state().
update_jwt_keys(State) ->
    lager:info("Init: reading JWT keys"),
    ok = watts_jwt_keys:initial_read(),
    Duration = round(?CONFIG(jwt_key_rotation_interval)/60),
    lager:info("Init: JWT key will rotate about every ~p minutes",
               [Duration]),
    State.



%% @doc start the web interface of WaTTS.
%% This starts the main web server with the API, the static file serving of
%% the java script SPA and, if configured, the documentations. The redirection
%% from http to https endpoint is also started if configured.
%%
%% To configure SSL first the files are read and if that fails SSL is disabled.
%% @see create_dispatch_list/0
%% @see add_options/5
-spec start_web_interface(state()) -> state().
start_web_interface(State) ->
    lager:info("Init: starting web interface"),
    State1 = maybe_start_web_queues(State),
    oidcc_client:register(watts_oidc_client),
    {DispatchList, State2} = create_dispatch_list(State1),
    Dispatch = cowboy_router:compile([{'_', DispatchList}]),
    SSL = ?CONFIG(ssl),
    {UseSSL, State3} = read_ssl_files(SSL, State2),
    ListenPort = ?CONFIG(listen_port),
    MaxConns = ?CONFIG(num_parallel_conns),
    NewState =
    case UseSSL of
        true ->
            Cert = ?CONFIG(cert),
            Key = ?CONFIG(key),
            BasicOptions =
                [ {port, ListenPort},
                  {max_connections, MaxConns},
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
                                        ),
            State3;
        false ->
            {LocalIp, State4} =
                case ?CONFIG(enable_ipv6) of
                    false ->
                        NewS = log_warning("listening only at 127.0.0.1",
                                          State3),
                        {{127, 0, 0, 1}, NewS};
                    true ->
                        NewS = log_warning("listening only at ::1",
                                          State3),
                        {{0, 0, 0, 0, 0, 0, 0, 1}, NewS}
                end,
            {ok, _} = cowboy:start_http( http_handler
                                         , ?CONFIG(num_acceptors)
                                         , [ {port, ListenPort},
                                             {ip, LocalIp},
                                             {max_connections, MaxConns}
                                             ]
                                         , [{env, [{dispatch, Dispatch}]}]
                                       ),
            State4
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
                                       , 5
                                       , [ {port, RedirectPort},
                                           {max_connections, 10} | IPv6]
                                       , [{env, [{dispatch, RedirDispatch}]}]
                                       );
        _ -> ok
    end,
    NewState.

%% @doc maybe start the queues for rate limits at the api and rsp endpoint
-spec maybe_start_web_queues(state()) -> state().
maybe_start_web_queues(State) ->
    S1 = maybe_start_web_queue(?CONFIG(web_connection_rate, undefined), State),
    S2 = maybe_start_rsp_queue(?CONFIG(enable_rsp, false),
                               ?CONFIG(rsp_connection_rate, undefined), S1),
    S2.

%% @doc decide if the rate limit for api endpoint needs to be started
-spec maybe_start_web_queue(any(), state()) -> state().
maybe_start_web_queue(Number, State) when is_integer(Number), Number > 0 ->
    MaxTime = ?CONFIG(web_queue_max_wait, 500),
    lager:info("Init: setting web rate limit to ~p/sec [max wait: ~p ms]",
               [Number, MaxTime]),
    ok = start_queue(watts_web_queue, Number, MaxTime),
    ?SETCONFIG(watts_web_queue, true),
    State;
maybe_start_web_queue(_, State) ->
    ?UNSETCONFIG(watts_web_queue),
    NewState = log_warning("connection rate set to unlimited!", State),
    NewState.

%% @doc decide if the rate limit for rsp endpoint needs to be started
-spec maybe_start_rsp_queue(boolean(), any(), state()) -> state().
maybe_start_rsp_queue(false, _, State) ->
    State;
maybe_start_rsp_queue(true, Number, State)
  when is_integer(Number), Number > 0 ->
    MaxTime = ?CONFIG(rsp_queue_max_wait, 1000),
    lager:info("Init: setting rsp rate limit to ~p/sec [max wait: ~p ms]",
               [Number, MaxTime]),
    ok = start_queue(watts_rsp_queue, Number, MaxTime),
    ?SETCONFIG(watts_rsp_queue, true),
    State;
maybe_start_rsp_queue(true, _, State) ->
    NewState = log_warning("rsp connection rate set to unlimited!", State),
    ?UNSETCONFIG(watts_rsp_queue),
    NewState.

%% @doc start a queue with the given name, rate limit and wait time.
-spec start_queue(atom(), integer(), integer()) -> ok.
-dialyzer({nowarn_function, start_queue/3}).
start_queue(Name, Limit, MaxTime) ->
    Options = [{counter, [{limit, Limit}]}, {type, fifo}, {max_time, MaxTime}],
    ok = jobs:add_queue(Name, Options),
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
-spec read_ssl_files(ShouldBeRead :: boolean(), state())
                    -> {UseSSL :: boolean(), state()}.
read_ssl_files(true, State0) ->
    {CertOK, State1} = read_certificate(?CONFIG_(cert_file), State0),
    {KeyOK, State2} = read_key(?CONFIG_(key_file), State1),
    {ChainOK, State3} = read_cachain(?CONFIG_(cachain_file), State2),
    {DhOK, State4} = read_dhparam(?CONFIG_(dh_file), State3),
    {CertOK and KeyOK and ChainOK and DhOK, State4};
read_ssl_files(_, State) ->
    {false, State}.

%% @doc read the certificate for SSL
-spec read_certificate(any(), state()) -> {Success :: boolean(), state()}.
read_certificate({ok, Path}, State) ->
    case watts_file_util:read_pem_entries(Path) of
        [{'Certificate', Certificate, not_encrypted}] ->
            ?SETCONFIG(cert, Certificate),
            {true, State};
        _ ->
            NewState = log_error("certificate ~p invalid", [Path], State),
            {false, NewState}
    end;
read_certificate(_, State) ->
    {false, State}.

%% @doc read the private key for SSL
-spec read_key(any(), state()) -> {Success :: boolean(), state()}.
read_key({ok, Path}, State) ->
    case watts_file_util:read_pem_entries(Path) of
        [{Type, PrivateKey, not_encrypted}]
        when Type == 'RSAPrivateKey'; Type == 'DSAPrivateKey';
             Type =='ECPrivateKey'; Type == 'PrivateKeyInfo'->
            ?SETCONFIG(key, {Type, PrivateKey}),
            {true, State};
        _ ->
            NewState = log_error("private key ~p invalid", [Path], State),
            {false, NewState}
    end;
read_key(_, State) ->
    {false, State}.

%% @doc read the ca chain for SSL
-spec read_cachain(any(), state()) -> {Success :: boolean(), state()}.
read_cachain({ok, Path}, State) ->
    case watts_file_util:read_pem_entries(Path) of
        [] ->
            NewState = log_error("ca chain ~p is empty", [Path], State),
            ?UNSETCONFIG(cachain),
            {false, NewState};
        PemCerts ->
            Decode = fun({'Certificate', Cert, not_encrypted}, List) ->
                             [ Cert | List ];
                        (_, List) ->
                             List
                     end,
            Certs = lists:foldl(Decode, [], PemCerts),
            ?SETCONFIG(cachain, Certs),
            {true, State}
    end;
read_cachain(_, State) ->
    NewState = log_warning("no ca-chain-file configured [cachain_file]!",
                           State),
    ?UNSETCONFIG(cachain),
    {true, NewState}.

%% @doc read the dh params for SSL
-spec read_dhparam(any(), state()) -> {Success :: boolean(), state()}.
read_dhparam({ok, none}, State) ->
    NewState = log_warning("no dh-file configured [dh_file]!", State),
    ?UNSETCONFIG(dhparam),
    {true, NewState};
read_dhparam({ok, Path}, State) ->
    case watts_file_util:read_pem_entries(Path) of
        [{'DHParameter', DhParam, not_encrypted}] ->
            ?SETCONFIG(dhparam, DhParam),
            {true, State};
        _ ->
            NewState =log_error("dh-file ~p is invalid", [Path], State),
            ?UNSETCONFIG(dhparam),
            {false, NewState}
    end;
read_dhparam(_, State) ->
    NewState = log_warning("no dh-file configured [dh_file]!", State),
    ?UNSETCONFIG(dhparam),
    {true, NewState}.

%% @doc This is the list of endpoints and the corresponding action to happen,
%% this could be either calling a function or serving static files.
%%
%% The function creates a basic dispatch list for the javascript to be served,
%% the OpenID connect handling (login) and the api.
%% Then calls the create_dispatch_list/2 function to configure the dynamic part.
%% @see create_dispatch_list/2
-spec create_dispatch_list(state()) -> {[tuple()], state()}.
create_dispatch_list(State) ->
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
                         BaseDispatchList, State).

%% @doc handle each configuration and transform it into a dispatch entry.
%% The handled settings include
%% <ul>
%% <li> user documentation </li>
%% <li> code documentation </li>
%% <li> rps endpoint </li>
%% <li> privacy statement </li>
%% </ul>
-spec create_dispatch_list(Config:: [tuple()], DispatchList :: [tuple()]
                          , state()) -> {[tuple()], state()}.
create_dispatch_list([], List, State) ->
     {List, State};
create_dispatch_list([{doc_user, true} | T], List, State) ->
    DocInfo = "Init: publishing user documentation at /docs/user/",
    lager:info(DocInfo),
    EpDocs = watts_http_util:relative_path("docs/user/[...]"),
    NewList = [ {EpDocs, cowboy_static, {priv_dir, ?APPLICATION, "docs/user"}}
                | List ],
    create_dispatch_list(T, NewList, State);
create_dispatch_list([{doc_code, true} | T], List, State) ->
    DocInfo = "Init: publishing code documentation at /docs/code/",
    lager:info(DocInfo),
    EpDocs = watts_http_util:relative_path("docs/code/[...]"),
    NewList = [ {EpDocs, cowboy_static, {priv_dir, ?APPLICATION, "docs/code"}}
                | List ],
    create_dispatch_list(T, NewList, State);
create_dispatch_list([{rsp, true, []} | T], List, State) ->
    lager:info("Init: relying service provider won't be enabled as none is "
               "configured"),
    create_dispatch_list(T, List, State);
create_dispatch_list([{rsp, true, _} | T], List, State) ->
    RspInfo = "Init: enable relying service provider at /rsp/",
    lager:info(RspInfo),
    EpRsp = watts_http_util:relative_path("rsp/[...]"),
    NewList = [ {EpRsp, watts_http_rsp, []} | List ],
    create_dispatch_list(T, NewList, State);
create_dispatch_list([{privacy, undefined} | T], List, State) ->
    EpPrivacy = watts_http_util:relative_path("privacystatement.html"),
    PrivWarn = "The privacy statement is not configured [~p]",
    NewState = log_warning(PrivWarn, [privacy_doc], State),
    NewList = [ { EpPrivacy , cowboy_static,
                  {priv_file, ?APPLICATION, "no_privacy.html"}
                }
                | List],
    create_dispatch_list(T, NewList, NewState);
create_dispatch_list([{privacy, File} | T], List, State) ->
    EpPrivacy = watts_http_util:relative_path("privacystatement.html"),
    PrivInfo = "Init: Using privacy statement ~p",
    lager:info(PrivInfo, [File]),
    NewList = [ {EpPrivacy, cowboy_static, {file, File}} | List],
    create_dispatch_list(T, NewList, State);
create_dispatch_list([_ | T], List, State) ->
    create_dispatch_list(T, List, State).



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
-spec do_stop(state()) -> state().
do_stop(State) ->
    lager:info("Init: done"),
    lager:info("Init: startup took ~p seconds", [startup_duration()]),
    NewState = check_mail(State),
    lager:info("WaTTS ready"),
    stop(self()),
    NewState.

%% @doc helperfunction to remove newlines from data
-spec remove_newline(string()) -> string().
remove_newline(List) ->
    Filter = fun($\n) ->
                     false;
                (_) ->
                     true
             end,
    lists:filter(Filter, List).

%% @doc check the email setting by sending one
-spec check_mail(state()) -> state().
check_mail(State) ->
    Subject = "I am up again (WaTTS)",
    Body = io_lib:format(
             "Hello Administrator, ~n~n"
             "This is WaTTS on ~p, I am build as version ~s.~n"
             "My startup took ~p seconds.~n"
             "This system is ~s and is ~s.~n~n"
             "~s~n"
             "~n~nI hope you have a great time, I will,~n"
             "WaTTS",
             [?CONFIG(hostname), ?CONFIG(vsn), startup_duration(),
              system_name(), system_uptime(), issues_to_body(State)]),
    MaybeAdminMail = ?CONFIG(admin_mail),
    Receipient = case is_list(MaybeAdminMail) of
                     true ->
                         [MaybeAdminMail];
                     false ->
                         []
                 end,
    case watts_mail:send(Subject, Body, Receipient) of
        ok ->
            lager:info("Init: email about initilization sent"),
            ok;
        disabled ->
            lager:info("Init: email sending disabled"),
            ok;
        no_receipients ->
            system_halt("Init: no admin mail configured but emails enabled", []
                       , 253);
        error ->
            system_halt("Init: mail failed, will shutdown", [], 252)
    end,
    State.

%% @doc get the output of uptime
-spec system_uptime() -> string().
system_uptime() ->
    os:cmd("uptime --pretty").

%% @doc convert the issues form the state to a email body
-spec issues_to_body(state()) -> string().
-dialyzer({nowarn_function, issues_to_body/1}).
issues_to_body(#state{issues = []}) ->
    "No issues of level 'warning' or higher, feels very good!";
issues_to_body(#state{issues = I}) ->
    Issues = lists:reverse(I),
    Text = io_lib:format(lists:flatten(watts_utils:lists_join("~n", Issues)),
                         []),
    io_lib:format("I found the following issues of level "
                  "'warning' or higher: ~n~s", [Text]).

%% @doc calculate the startup duration
-spec startup_duration() -> integer().
startup_duration() ->
    erlang:system_time(seconds) - ?CONFIG(start_time).


%% @doc log a warning  message and store it in the state for email
-spec log_warning(string(), [any()], state()) -> state().
log_warning(Msg, Params, State) ->
    Message = io_lib:format(Msg, Params),
    log_warning(Message, State).

%% @doc log a warning  message and store it in the state for email
-spec log_warning(string(), state()) -> state().
log_warning(Message, State) ->
    lager:warning("Init: " ++ Message),
    message_to_state(Message, State).

%% @doc log an error message and store it in the state for email
-spec log_error(string(), [any()], state()) -> state().
log_error(Msg, Params, State) ->
    Message = io_lib:format(Msg, Params),
    log_error(Message, State).

%% @doc log an error message and store it in the state for email
-spec log_error(string(), state()) -> state().
log_error(Message, State) ->
    lager:error("Init: " ++ Message),
    message_to_state(Message, State).

%% @doc insert the message into the  issues list of the state
-spec message_to_state(string(), state()) -> state().
message_to_state(Message, #state{issues = Issues} = State) ->
    State#state{issues = [Message | Issues]}.

%% @doc get the name of the system, like debian
-spec system_name() -> list().
system_name() ->
    MaybeFileData = file:read_file("/etc/os-release"),
    binary_to_list(handle_system_file(MaybeFileData)).

%% @doc handle the content of the /etc/os-release file or reading errors
-spec handle_system_file({ok, binary()} | any()) -> binary().
handle_system_file({ok, Binary}) ->
    Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
    Info = fun(Line, Current) ->
                   case binary:split(Line, <<"=">>, [global, trim_all]) of
                       [<<"PRETTY_NAME">>, SystemName] ->
                           SystemName;
                       _ ->
                           Current
                   end
           end,
    lists:foldl(Info, <<"unknown (not found pretty line)">>, Lines);
handle_system_file(_) ->
    <<"unknown (could not read /etc/os-release)">>.


-ifndef(TEST).
%% @doc halt the system with a message and a number.
%% The halt is delayed for a second to ensure the logs get written.
-spec system_halt(string(), [any()], integer()) -> ok.
-dialyzer({nowarn_function, system_halt/3}).
system_halt(Message, Params, Number) ->
    lager:critical(Message, Params),
    timer:sleep(1000),
    erlang:halt(Number),
    ok.

-else.
%% @doc the system halt for testing purposes.
%% So the tests (the vm) are not halted by this function
-spec system_halt(string(), [any()], integer()) -> ok.
system_halt(Message, Params, Number) ->
    Msg = io_lib:format(Message, Params),
    io:format("would halt system with ~p due to ~s", [Number, Msg]),
    ok.

-endif.
