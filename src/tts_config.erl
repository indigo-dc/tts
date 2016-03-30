-module(tts_config).
-behaviour(gen_server).

-include("tts.hrl").

% Path in REVERSE order
-define(GLOBALPATH,[
                    "/usr/local/etc/tts/",
                    "/etc/tts/" 
                   ]).
-define(HOMEPATH,".config/tts").

%% API.
-export([start_link/0]).
-export([is_loaded/0]).
-export([debug_mode/0]).
-export([get_/1]).
-export([get_/2]).
-export([local_port/0]).
-export([local_protocol/0]).
-export([reload/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
}).

-define(MAIN_CONFIG_FILE,"main.conf").
-define(OIDC_SECTION,"OIDC").

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).


reload() ->
    tts_sup:restart_config().

get_(Key) ->
    case application:get_env(tts,Key) of 
        {ok, Val} -> Val;
        _ -> undefined
    end.
          
get_(Key, Default) ->
    application:get_env(tts,Key,Default).


-define(CONF_LOADED,conf_loaded).
is_loaded() ->
    case get_(?CONF_LOADED) of
        true -> true;
        _ -> false 
    end.

-define(CONF_DEBUG_MODE,debug_mode).
debug_mode() ->
    case get_(?CONF_DEBUG_MODE) of
        true -> true;
        _ -> false 
    end.


local_port() ->
    return_port(?CONFIG(port),?CONFIG(ssl)).
return_port(<<"default">>,_) ->
    <<"">>;
return_port(<<"443">>, true) ->
    <<"">>;
return_port(<<"80">>, false) ->
    <<"">>;
return_port(Port,_) ->
    << <<":">>/binary, Port/binary>>.

local_protocol() ->
    return_http(?CONFIG(ssl)).
return_http(false) ->
    <<"http://">>;
return_http(_) ->
    <<"https://">>.



%% gen_server.

init([]) ->
	{ok, #state{}, 1}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(timeout, _State) ->
    % stop depending services
    stop_services(), 
    % clear config path 
    clear_config(), 
    % (re)read the configuration
    read_configs(),
    trigger_services(),
    {stop, normal, #state{}};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% config functionality
%%%


stop_services() ->
    ok = tts_session_mgr:close_all_sessions(),
    ok = tts_user_cache:clear_cache(),
    ok.

clear_config() ->
    application:unset_env(tts,?CONF_LOADED),
    application:unset_env(tts,config_path),
    ok.

read_configs() -> 
    ok = read_main_config(),
    ok = read_service_configs(),
    ok = update_status().

trigger_services() ->
    ok = tts_idh:reconfigure(),
    ok = tts_data_sqlite:reconfigure(),
    ok = start_cowboy().

read_main_config() ->
    Files = generate_file_list(?MAIN_CONFIG_FILE),
    ok = register_files(main,Files),
    ok = apply_main_settings().

read_service_configs() ->
    read_service_configs(get_(service_config_path, undefined)). 

read_service_configs(undefined) ->
    ok;
read_service_configs(BasePath) ->
    ServiceConfs = filelib:wildcard(filename:join([BasePath,"*.conf"])), 
    parse_and_apply_services(ServiceConfs), 
    ok.


update_status() ->
    Value = case get_(config_path, undefined) of
        undefined -> false;
        _ -> true
            end, 
    set_config(?CONF_LOADED,Value),
    ok.

-define(MAIN_SETTINGS,[
                    {"DebugMode",?CONF_DEBUG_MODE,boolean,false},
                    {"HostName",hostname,binary,"localhost"},
                    {"Port",port,binary,"default"},
                    {"ListenPort",listen_port,binary,"default"},
                    {"EpRedirect",ep_redirect,binary,"/oidc/redirect"},
                    {"EpReturn",ep_return,binary,"/oidc/return"},
                    {"EpMain",ep_main,binary,"/"},
                    {"EpUser",ep_user,binary,"/user"},
                    {"EpApi",ep_api,binary,"/api"},
                    {"SSL",ssl,boolean,true},
                    {"CaCertFile",ca_cert_file,file,"cert/ca.cert"},
                    {"CertFile",cert_file,file,"cert/tts.cert"},
                    {"KeyFile",key_file,file,"cert/tts.key"},
                    {"LogLevel",log_level,string,"Warning"},
                    {"SqliteFile",sqlite_db,file,"./tts.db"},
                    {"LogFile",log_file,binary,"tts.log"},
                    {"SessionTimeout",session_timeout,seconds,600},
                    {"CacheTimeout",cache_timeout,seconds,900},
                    {"CacheCheckInterval",cache_check_interval,seconds,300},
                    {"CacheMaxEntries",cache_max_entries,integer,50000},
                    {"ServiceConfigPath",service_config_path,directory,"./services"},
                    {"IDHScript",idh_script,file,"./idh.py"},
                    {"IDHMaxWorker",idh_max_worker,integer,5}
                ]).

-define(OIDC_SETTINGS,[
                    {"Id",binary,""},
                    {"Description",binary,""},
                    {"ClientId",binary,""},
                    {"Secret",binary,""},
                    {"ConfigEndpoint",binary,""}
                ]).


apply_main_settings() ->
    apply_existing_main_config(?CONFIG(config_path,undefined)).

apply_existing_main_config(undefined) ->
    ok;
apply_existing_main_config(_) ->
    apply_settings(main,"",?MAIN_SETTINGS),
    EpReturn = ?CONFIG(ep_return),
    HostName = ?CONFIG(hostname),
    LProt = local_protocol(),
    LPort = local_port(),
    LocalEndpoint = << LProt/binary, HostName/binary, LPort/binary, EpReturn/binary >>, 
    set_config(local_endpoint,LocalEndpoint),
    apply_oidc_settings(),
    ok.


apply_oidc_settings() ->
    Settings = get_values(main,?OIDC_SECTION,?OIDC_SETTINGS),
    IsEmpty = fun(V,In) ->
                      case V of
                          <<>> -> true;
                          "" -> true;
                          _ -> In
                      end
              end,
    case lists:foldl(IsEmpty,false,Settings) of
        false -> 
            [Name, Desc, ClientId, Secret, ConfigEndpoint] = Settings,
            LocalEndpoint = ?CONFIG(local_endpoint),
            {ok, _InternalId, _Pid} = oidcc:add_openid_provider(Name, Desc, ClientId,
                                      Secret,
                                      ConfigEndpoint, 
                                      LocalEndpoint);
        true ->
            %TODO: write some log about not adding the OIDC
            ok
    end.


    
apply_settings(_Name,_Section,[]) ->
    ok;
apply_settings(Name,Section,[{Key, AtomKey, Type, Default} | T]) ->
    apply_setting(Name, Section, Key, AtomKey, Type, Default),
    apply_settings(Name, Section, T).

apply_setting(Name,Section,Key,AtomKey,Type,Default) -> 
    Value = get_value(Name, Section, Key,Type, Default),
    set_config(AtomKey,Value).

get_values(Name,Section,List) ->
    get_values(Name,Section,List,[]).
get_values(_Name,_Section,[],List) ->
    lists:reverse(List);
get_values(Name,Section,[{Key,Type,Default}|T],List) ->
    Value = get_value(Name,Section,Key,Type,Default),
    get_values(Name, Section, T, [Value | List]).


get_value(Name,Section,Key,boolean,Default) -> 
    econfig:get_boolean(Name, Section, Key, Default);
get_value(Name,Section,Key,binary,Default) ->
    binary:list_to_bin(econfig:get_value(Name,Section,Key,Default));
get_value(Name,Section,Key,string,Default) ->
    econfig:get_value(Name,Section,Key,Default);
get_value(Name,Section,Key,directory,Default) ->
    Value = econfig:get_value(Name,Section,Key,Default),
    tts_file_util:to_abs(Value);
get_value(Name,Section,Key,file,Default) ->
    Value = econfig:get_value(Name,Section,Key,Default),
    tts_file_util:to_abs(Value);
get_value(Name,Section,Key,seconds,Default) ->
    Value = econfig:get_integer(Name,Section,Key,Default),
    Value*1000;
get_value(Name,Section,Key,integer,Default) ->
    econfig:get_integer(Name,Section,Key,Default);
get_value(Name,Section,Key,atom,Default) ->
    StringValue = econfig:get_value(Name,Section,Key),
    try list_to_existing_atom(StringValue) of
        V -> V
    catch
        _:_ -> Default 
    end.


parse_and_apply_services([]) ->
    ok;
parse_and_apply_services([ConfigFile|Tail])  ->
    ok = econfig:register_config(service,[ConfigFile]),
    ServiceConfig = econfig:get_value(service,""),
    ConfigMap = maps:from_list(ServiceConfig), 
    tts_service:add(ConfigMap), 
    ok = econfig:unregister_config(service), 
    parse_and_apply_services(Tail).

register_files(Name,Files) ->
    register_single_config(Name,only_first(Files)).  

register_single_config(_Name, "") ->
    ok;
register_single_config(Name, File) ->
    ok = econfig:register_config(Name,[File]),
    set_config(config_path,filename:dirname(File)),
    ok.

generate_file_list(File) ->
    use_or_generate_file_list(get_(config_path,undefined), File).

use_or_generate_file_list(undefined,File) ->
    append_filename(File, generate_path_list());
use_or_generate_file_list(ConfigPath,File) ->
    append_filename(File, [ConfigPath]).


generate_path_list() ->
    Home = init:get_argument(home),
    generate_path_list(Home).
generate_path_list({ok,[[HomeDir]] }) ->
    LocalPath = filename:join(HomeDir,?HOMEPATH),
    [LocalPath | ?GLOBALPATH];
generate_path_list(_) ->
    ?GLOBALPATH.


append_filename(File,Paths) ->
    AppendFile = fun(Path, List) ->
                         [filename:join(Path, File) | List]
                 end,
    lists:foldl(AppendFile,[],Paths).

only_first(Files) ->
    GetFirst = fun(File, Found) ->
                       case Found of 
                           "" -> 
                               case filelib:is_file(File) of
                                   true -> File;
                                   _ -> Found
                               end;
                           _ -> Found
                       end
               end,
    lists:foldl(GetFirst,"",Files).


start_cowboy() ->
    ok = start_cowboy(is_loaded()),
    ok.

start_cowboy(false) ->
    Dispatch = [{'_', [
                       {"/static/[...]", cowboy_static, {priv_dir, tts, "http_static"} },
                       {"/",tts_http_prep, []}
                      ]}],
    _ = cowboy:start_http( http_handler 
                                 , 100
                                 , [ {port, 8080} ]
                                 , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
                               ),
    ok;
start_cowboy(_) ->
    cowboy:stop_listener(http_handler),
    EpMain = ?CONFIG(ep_main),
    EpRedirect = ?CONFIG(ep_redirect),
    EpReturn = ?CONFIG(ep_return),
    EpApi = ?CONFIG(ep_api),
    EpUser = ?CONFIG(ep_user),
    Dispatch = [{'_', [
                       {"/static/[...]", cowboy_static, {priv_dir, tts, "http_static"} },
                       {EpApi,tts_rest, []},
                       {EpMain, tts_http_prep, []},
                       {EpUser, tts_http_prep, []},
                       {EpRedirect, tts_http_prep, []},
                       {EpReturn, tts_http_prep, []}
                      ]}],

    SSL = ?CONFIG(ssl),
    ListenPort = listen_port(),
    
    case SSL of 
        false -> 
            {ok, _} = cowboy:start_http( http_handler 
                                         , 100
                                         , [ {port, ListenPort} ]
                                         , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
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
                                          , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
                                        )
    end,
    ok.


set_config(config_path=Key, Path) ->
   case application:get_env(tts,Key,undefined) of
       undefined ->
           application:set_env(tts,Key,Path);
       _ -> ok
   end;
set_config(Key, Value) ->
    application:set_env(tts,Key,Value).



listen_port() ->
    listen_port(?CONFIG(listen_port), ?CONFIG(port),?CONFIG(ssl)).
listen_port(<<"default">>,<<"default">>, true) ->
    443;
listen_port(<<"default">>,<<"default">>, false) ->
    80;
listen_port(<<"default">>,Port, _) ->
    erlang:binary_to_integer(Port);
listen_port(ListenPort,_, _) ->
    erlang:binary_to_integer(ListenPort).

    
