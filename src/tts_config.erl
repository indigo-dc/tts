-module(tts_config).
-behaviour(gen_server).

-include("tts.hrl").
-include_lib("public_key/include/public_key.hrl").

% Path in REVERSE order
-define(GLOBALPATH,[
                    "/usr/local/etc/tts/",
                    "/etc/tts/" 
                   ]).
-define(HOMEPATH,".config/tts").

%% API.
-export([start_link/0]).
-export([get_/1]).
-export([get_/2]).
-export([local_port/0]).
-export([local_protocol/0]).

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


get_(Key) ->
    {ok, Val} = application:get_env(tts,Key),
    Val.
          
get_(Key, Default) ->
    application:get_env(tts,Key,Default).

local_port() ->
    return_port(?CONFIG(port)).
return_port(<<"default">>) ->
    <<"">>;
return_port(Port) ->
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

handle_info(timeout, State) ->
    % read the configuration
    read_configs(),
	{stop, normal, State};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



%%% config functionality
%%%

read_configs() -> 
    read_main_config(),
    read_other_configs().


read_other_configs() ->
    read_other_configs(get_(config_path, undefined)). 

read_other_configs(undefined) ->
    error_logger:error_msg("no configuration path found, path scanned are
                           ~p",[generate_path_list()]),
    erlang:halt(2);
read_other_configs(Path) ->
    ok = apply_main_settings(),
    ok = read_op_config(Path),
    ok = set_oidc_ops(),
    ok = start_cowboy(),
    ok.


read_main_config() ->
    Files = generate_file_list("main.conf"),
    register_files(main,Files).

apply_main_settings() ->
    HostName = get_binary_value(main,"","HostName","localhost"),
    Port = get_binary_value(main,"","Port","default"),
    ListenPort = get_binary_value(main,"","ListenPort","default"),
    EpRedirect = get_binary_value(main,"","EpRedirect", "/oidc/redirect"),
    EpReturn = get_binary_value(main,"","EpReturn", "/oidc/return"),
    EpMain = get_binary_value(main,"","EpMain", "/"),
    EpApi = get_binary_value(main,"","EpApi", "/api"),
    SSL = get_boolean_value(main,"","SSL", true),
    LogLevel = get_string_value(main,"","LogLevel", "Warning"),
    LogFile = get_binary_value(main,"","LogFile", "tts.log"),
   
    set_config(log_level,LogLevel),
    set_config(log_file,LogFile),

    set_config(hostname,HostName),
    set_config(port,Port),
    set_config(listen_port,ListenPort),
    set_config(ep_main,EpMain),
    set_config(ep_api,EpApi),
    set_config(ep_redirect,EpRedirect),
    set_config(ep_return,EpReturn),
    set_config(ssl,SSL),
    LProt = local_protocol(),
    LPort = local_port(),
    LocalEndpoint = << LProt/binary, HostName/binary, LPort/binary, EpReturn/binary >>, 
    set_config(local_endpoint,LocalEndpoint),
    ok.

read_op_config(Path) ->
    Files = [filename:join(Path,"oidc_provider.conf")],
    register_files(oidc_op,Files).
    

set_oidc_ops() ->
    OPs = econfig:sections(oidc_op),
    IsEmpty = fun({K, V},In) ->
                      case V of
                          <<>> -> [K | In];
                          "" -> [K | In];
                          _ -> In
                      end
              end,
    AddValidOps = fun(OpId,_List) ->
                          Id = binary:list_to_bin(OpId),
                          Desc = get_binary_value(oidc_op,OpId,"Description",""),
                          ClientId = get_binary_value(oidc_op,OpId,"ClientId",""),
                          Secret = get_binary_value(oidc_op,OpId,"Secret",""),
                          ConfigEndpoint = get_binary_value(oidc_op,OpId,"ConfigEndpoint",""),
                          LocalEndpoint = ?CONFIG(local_endpoint),
                          OpEntries = [{"Description", Desc}, 
                                       {"ClientId" ,ClientId}, 
                                       {"Secret" ,Secret}, 
                                       {"ConfigEndpoint" ,ConfigEndpoint}],
                          case lists:foldl(IsEmpty,[],OpEntries) of
                              [] -> 
                                  oidcc:add_openid_provider(Id, Desc, ClientId,
                                                            Secret,
                                                            ConfigEndpoint, 
                                                            LocalEndpoint);
                              _List -> noop
                          end
                  end,
    lists:foldl(AddValidOps,[],OPs).
    

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

get_string_value(Name,Section,Key,Default) ->
    econfig:get_value(Name,Section,Key,Default).

get_binary_value(Name,Section,Key,Default) ->
    binary:list_to_bin(get_string_value(Name,Section,Key,Default)).

get_boolean_value(Name, Section, Key, Default) ->
    econfig:get_boolean(Name, Section, Key, Default).

start_cowboy() ->
    EpMain = ?CONFIG(ep_main),
    EpRedirect = ?CONFIG(ep_redirect),
    EpReturn = ?CONFIG(ep_return),
    EpApi = ?CONFIG(ep_api),
    Dispatch = [{'_', [
                       {EpApi,tts_rest, []},
                       {EpMain, tts_http, []},
                       {EpRedirect, tts_http, []},
                       {EpReturn, tts_http, []}
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
            {ok, _} = cowboy:start_https( http_handler 
                                          , 100
                                          , [ {port, ListenPort} ]
                                          , [{env, [{dispatch, cowboy_router:compile(Dispatch)}]}]
                                        )
    end,
    ok.


set_config(log_file, FileName) ->
    _AbsFileName = get_abs_file(FileName);
set_config(Key, Value) ->
    application:set_env(tts,Key,Value).

get_abs_file(FileName) ->
    get_abs_file(is_absolute(FileName), FileName).

get_abs_file(true,FileName) ->
    FileName;
get_abs_file(false,FileName) ->
    filename:join(get_(config_path),FileName).

is_absolute(FileName) when is_binary(FileName) ->
   binary:part(FileName,{0,1}) == <<"/">>;
is_absolute(["/"| _])  ->
    true;
is_absolute([_|_])  ->
    false.

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

    
