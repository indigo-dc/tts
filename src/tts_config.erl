-module(tts_config). 

-include("tts.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([read_configs/0]).
-export([get_/1]).
-export([get_/2]).
-export([local_port/0]).
-export([local_protocol/0]).



% Path in REVERSE order
-define(GLOBALPATH,[
                    "/usr/local/etc/tts/",
                    "/etc/tts/" 
                   ]).
-define(HOMEPATH,".config/tts").

-spec read_configs() -> ok.
read_configs() -> 
    read_main_config(),
    read_other_configs().


read_other_configs() ->
    read_other_configs(get_(config_path, undefined)). 

read_other_configs(undefined) ->
    erlang:error(no_config_path);
read_other_configs(Path) ->
    apply_main_settings(),
    read_op_config(Path),
    set_oidc_ops(),
    ok.


get_(Key) ->
    {ok, Val} = application:get_env(tts,Key),
    Val.
          
get_(Key, Default) ->
    application:get_env(tts,Key,Default).

read_main_config() ->
    Files = generate_file_list("main.conf"),
    register_files(main,Files).

apply_main_settings() ->
    HostName = get_binary_value(main,"","HostName","localhost"),
    Port = get_binary_value(main,"","Port","default"),
    EpRedirect = get_binary_value(main,"","EpRedirect", "/oidc/redirect"),
    EpReturn = get_binary_value(main,"","EpReturn", "/oidc/return"),
    EpMain = get_binary_value(main,"","EpMain", "/"),
    SSL = get_binary_value(main,"","SSL", "true"),
    LogLevel = get_string_value(main,"","LogLevel", "Warning"),
    LogFile = get_binary_value(main,"","LogFile", "tts.log"),
   
    set_config(log_level,LogLevel),
    set_config(log_file,LogFile),

    set_config(hostname,HostName),
    set_config(port,Port),
    set_config(ep_main,EpMain),
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
    erlang:error(config_missing),
    ok;
register_single_config(Name, File) ->
    ok = econfig:register_config(Name,[File]),
    set_config(config_path,filename:dirname(File)),
    ok.

generate_file_list(File) ->
    use_or_generate_file_list(get_(config_path,undefined), File).

use_or_generate_file_list(undefined,File) ->
    Home = init:get_argument(home),
    generate_list_with_home(File,Home);
use_or_generate_file_list(ConfigPath,File) ->
    [filename:join(ConfigPath,File)].


generate_list_with_home(File,{ok, [[HomeDir]]}) ->
    LocalPath = filename:join(HomeDir,?HOMEPATH),
    append_filename(File,[LocalPath | ?GLOBALPATH]);
generate_list_with_home(File, _) ->
   append_filename(File,?GLOBALPATH).

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

local_protocol() ->
    return_http(?CONFIG(ssl)).
return_http(<<"false">>) ->
    <<"http://">>;
return_http(_) ->
    <<"https://">>.

local_port() ->
    return_port(?CONFIG(port)).
return_port(<<"default">>) ->
    <<"">>;
return_port(Port) ->
    << <<":">>/binary, Port/binary>>.

