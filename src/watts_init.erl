-module(watts_init).
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
    gen_server:cast(self(), add_rsps),
    {noreply, State};
handle_cast(add_rsps, State) ->
    add_rsps(),
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
    lager:info("Init: WaTTS version ~p", [Vsn]),
    ok = enforce_security(),
    lager:debug("Init: config = ~p", [?ALLCONFIG]),
    ok.

enforce_security() ->
    SSL = ?CONFIG(ssl),
    Hostname0 = ?CONFIG(hostname),
    Onion = lists:suffix(".onion", Hostname0),
    Hostname = maybe_change_hostname(SSL, Onion, Hostname0),
    ?SETCONFIG(hostname, Hostname),
    ok = error_if_running_as_root(),
    ok.

maybe_change_hostname(false, false, _) ->
    H = "localhost",
    lager:warning("Init: Neither SSL nor Tor is configured; "
                  "hostname set to ~p", [H]),
    H;
maybe_change_hostname(_, _, Hostname) ->
    Hostname.

error_if_running_as_root() ->
    Uid = list_to_integer(remove_newline(os:cmd("id -u"))),
    User = remove_newline(os:cmd("id -un")),
    ok = maybe_root_error(User, Uid).

maybe_root_error(User, Uid) when User == "root"; Uid == 0 ->
    lager:critical("Init: do not run WaTTS as root, stopping"),
    error;
maybe_root_error(User, Uid) ->
    lager:info("Init: running as user ~p [~p]", [User, Uid]),
    ok.


start_database() ->
    lager:info("Init: starting ets database"),
    ok = watts_data:init(),
    ok = watts_persistent:init(),
    case watts_persistent:is_ready() of
        ok -> ok;
        {error, R} ->
            Msg = io_lib:format("unable to start persistence layer: ~p", [R]),
            lager:critical(Msg),
            erlang:error(no_database)
    end,
    ok.


add_openid_provider() ->
    lager:info("Init: adding openid provider"),
    %% force only one try
    application:set_env(oidcc, provider_max_tries, 1),
    LocalEndpoint = local_endpoint(),
    ?SETCONFIG(local_endpoint, LocalEndpoint),
    lager:info("Init: using local endpoint ~p", [LocalEndpoint]),
    ProviderList = ?CONFIG(provider_list, []),
    ok = add_openid_provider(ProviderList, LocalEndpoint),
    wait_and_log_provider_results(),
    ok.

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
        end,
        add_openid_provider(T, LocalEndpoint)
    catch Error:Reason ->
            Msg = "Init: error occured OpenId Connect provider ~p: '~p' ~p",
            lager:critical(Msg, [Id, Error, Reason])
    end;
add_openid_provider([], _) ->
    ok.


wait_and_log_provider_results() ->
    {ok, List} = oidcc:get_openid_provider_list(),
    Max = erlang:system_time(seconds) + ?CONFIG(max_provider_wait, 5),
    ok = wait_and_log_provider_results(List, [], Max),
    ok.

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




add_rsps() ->
    lager:info("Init: adding relying service provider (RSP)"),
    UpdateRsp =
        fun(#{id := Id} = Config, List) ->
                case watts_rsp:new(Config) of
                    {ok, Rsp} ->
                        lager:info("Init: added RSP ~p", [Id]),
                        [ Rsp | List ];
                    {error, Reason} ->
                        Msg = "Init: unable to add keys of RSP ~p: ~p",
                        lager:critical(Msg, [Id, Reason]),
                        List
                end
        end,
    NewRspList = lists:foldl(UpdateRsp, [], ?CONFIG(rsp_list, [])),
    ?SETCONFIG(rsp_list, NewRspList),
    ok.



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



start_web_interface() ->
    lager:info("Init: starting web interface"),
    oidcc_client:register(watts_oidc_client),
    Dispatch = cowboy_router:compile([{'_', create_dispatch_list()}]),
    SSL = ?CONFIG(ssl),
    ListenPort = ?CONFIG(listen_port),
    case SSL of
        true ->
            CertFile = ?CONFIG(cert_file),
            KeyFile = ?CONFIG(key_file),
            BasicOptions =
                [ {port, ListenPort},
                  {certfile, CertFile},
                  {keyfile, KeyFile}
                ],
            Options = add_options(BasicOptions, ?CONFIG_(cachain_file),
                                  ?CONFIG_(dh_file), ?CONFIG_(hostname),
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
    create_dispatch_list([{docs, ?CONFIG(enable_docs)},
                          {rsp, ?CONFIG(enable_rsp), ?CONFIG(rsp_list)},
                          {privacy, ?CONFIG(privacy_doc)} ],
                         BaseDispatchList).

create_dispatch_list([], List) ->
     List;
create_dispatch_list([{docs, true} | T], List) ->
    DocInfo = "Init: publishing documentation at /docs/",
    lager:info(DocInfo),
    EpDocs = watts_http_util:relative_path("docs/[...]"),
    NewList = [ {EpDocs, cowboy_static, {priv_dir, ?APPLICATION, "docs"}}
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




add_options(Options, {ok, CaChainFile}, DhFile, Hostname, IPv6) ->
    NewOptions = [ {cacertfile, CaChainFile} | Options ],
    add_options(NewOptions, ok, DhFile, Hostname, IPv6);
add_options(Options, undefined, DhFile, Hostname, IPv6) ->
    lager:warning("Init: no ca-chain-file configured [cachain_file]!"),
    add_options(Options, ok, DhFile, Hostname, IPv6);
add_options(Options, CaChainFile,  {ok, none}, Hostname, IPv6) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    add_options(Options, CaChainFile, ok, Hostname, IPv6);
add_options(Options, CaChainFile,  undefined, Hostname, IPv6) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    add_options(Options, CaChainFile, ok, Hostname, IPv6);
add_options(Options, CaChainFile, {ok, DhFile}, Hostname, IPv6) ->
    NewOptions = [ {dhfile, DhFile} | Options ],
    add_options(NewOptions, CaChainFile, ok, Hostname, IPv6);
add_options(Options, CaChainFile, DhFile, {ok, Hostname}, IPv6) ->
    NewOptions =
        case {lists:suffix(".onion", Hostname), IPv6} of
            {true, true} ->
                lager:info("Init: listening only at ::1 (onion)"),
                [ {ip, {0, 0, 0, 0, 0, 0, 0, 1}} | Options ];
            {true, false} ->
                lager:info("Init: listening only at 127.0.0.1 (onion)"),
                [ {ip, {127, 0, 0, 1}} | Options ];
            _ ->
                Options
        end,
    add_options(NewOptions, CaChainFile, DhFile, ok, ok);
add_options(Options, CaChainFile, DhFile, Hostname, {ok, true}) ->
    NewOptions =  [ inet6 | Options],
    add_options(NewOptions, CaChainFile, DhFile, Hostname, ok);
add_options(Options, CaChainFile, DhFile, Hostname, {ok, false}) ->
    NewOptions =  [ inet | Options],
    add_options(NewOptions, CaChainFile, DhFile, Hostname, ok);
add_options(Options, _, _, _, _) ->
    Options.





local_endpoint() ->
    watts_http_util:whole_url(watts_http_util:relative_path("oidc")).


stop() ->
    lager:info("Init: done"),
    lager:info("Init: startup took ~p seconds",
               [erlang:system_time(seconds) - ?CONFIG(start_time)]),
    lager:info("WaTTS ready"),
    stop(self()).

remove_newline(List) ->
    Filter = fun($\n) ->
                     false;
                (_) ->
                     true
             end,
    lists:filter(Filter, List).
