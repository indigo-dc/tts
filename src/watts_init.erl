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
    lager:info("Init: WaTTS version ~p", [Vsn]),
    ok = enforce_security(),
    lager:debug("Init: config = ~p", [?ALLCONFIG]),
    ok.

enforce_security() ->
    SSL = ?CONFIG(ssl),
    Hostname0 = ?CONFIG(hostname),
    Hostname = case SSL of
                   false ->
                       "localhost";
                   _ ->
                       Hostname0
               end,
    case Hostname == Hostname0 of
        false ->
            lager:warning("Init: SSL is not configured; change hostname to ~p",
                          [Hostname]);
        true -> ok
    end,
    ?SETCONFIG(hostname, Hostname),
    Uid = list_to_integer(remove_newline(os:cmd("id -u"))),
    User = remove_newline(os:cmd("id -un")),
    ok = case (Uid == 0) or (User == "root") of
             true ->
                 lager:critical("Init: do not run WaTTS as root, stopping"),
                 error;
             false ->
                 lager:info("Init: running as user ~p [~p]", [User, Uid]),
                 ok
         end,
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
    ok = wait_and_log_provider_results(List, []),
    ok.

wait_and_log_provider_results([], []) ->
    ok;
wait_and_log_provider_results([], List) ->
    timer:sleep(200),
    wait_and_log_provider_results(List, []);
wait_and_log_provider_results([{Id, Pid} = H | T], List) ->
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
    wait_and_log_provider_results(T, NewList).



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
    PrivWarn = "Init: The privacy statement is not configured [~p]",
    PrivInfo = "Init: Using privacy statement ~p",
    EpMain = ?CONFIG(ep_main),
    EpOidc = watts_http_util:relative_path("oidc"),
    EpStatic = watts_http_util:relative_path("static/[...]"),
    EpApi = watts_http_util:relative_path("api/[...]"),
    EpPrivacy = watts_http_util:relative_path("privacystatement.html"),
    PrivacyFile = case ?CONFIG(privacy_doc) of
                      undefined ->
                          lager:warning(PrivWarn, [privacy_doc]),
                          {priv_file, ?APPLICATION, "no_privacy.html"};
                      File ->
                          lager:info(PrivInfo, [File]),
                          {file, File}
                  end,

    BaseDispatchList = [{EpStatic, cowboy_static,
                         {priv_dir, ?APPLICATION, "http_static"}
                        },
                        {EpApi, watts_rest, []},
                        {EpMain, cowboy_static,
                         {priv_file, ?APPLICATION, "http_static/index.html"}},
                        {EpPrivacy, cowboy_static, PrivacyFile},
                        {EpOidc, oidcc_cowboy, []}
                       ],

    %% the documentation
    EpDocs = watts_http_util:relative_path("docs/[...]"),
    EnableDocs = ?CONFIG(enable_docs),
    DocInfo = "Init: publishing documentation at /docs/",
    DispatchList = case EnableDocs of
                       false ->
                           BaseDispatchList;
                       _ ->
                           lager:info(DocInfo),
                           [ {EpDocs, cowboy_static,
                              {priv_dir, ?APPLICATION, "docs"} }
                           ] ++ BaseDispatchList
                   end,
    Dispatch = cowboy_router:compile([{'_', DispatchList}]),
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
                                  ?CONFIG_(dh_file)),
            {ok, _} = cowboy:start_https( http_handler
                                          , 100
                                          , Options
                                          , [{env, [{dispatch, Dispatch}]}]
                                        );
        false ->
            lager:warning("Init: listening only at 127.0.0.1"),
            {ok, _} = cowboy:start_http( http_handler
                                         , 100
                                         , [ {port, ListenPort},
                                             {ip, {127, 0, 0, 1}}
                                             ]
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
    case Redirect and SSL of
        true ->
            lager:info("Init: enable redirection at port ~p", [RedirectPort]),
            {ok, _} = cowboy:start_http( redirect_handler
                                          , 100
                                          , [ {port, RedirectPort}]
                                          , [{env, [{dispatch, RedirDispatch}]}]
                                        );
        _ -> ok
    end,

    ok.

add_options(Options, {ok, CaChainFile}, DhFile) ->
    NewOptions = [ {cacertfile, CaChainFile} | Options ],
    add_options(NewOptions, ok, DhFile);
add_options(Options, undefined, DhFile) ->
    lager:warning("Init: no ca-chain-file configured [cachain_file]!"),
    add_options(Options, ok, DhFile);
add_options(Options, CaChainFile,  {ok, none}) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    add_options(Options, CaChainFile, ok);
add_options(Options, CaChainFile,  undefined) ->
    lager:warning("Init: no dh-file configured [dh_file]!"),
    add_options(Options, CaChainFile, ok);
add_options(Options, CaChainFile, {ok, DhFile}) ->
    NewOptions = [ {dhfile, DhFile} | Options ],
    add_options(NewOptions, CaChainFile, ok);
add_options(Options, _, _) ->
    Options.





local_endpoint() ->
    watts_http_util:whole_url(watts_http_util:relative_path("oidc")).


stop() ->
    lager:info("Init: done"),
    lager:info("WaTTS ready"),
    stop(self()).

remove_newline(List) ->
    Filter = fun($\n) ->
                     false;
                (_) ->
                     true
             end,
    lists:filter(Filter, List).
