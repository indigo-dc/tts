-module(tts_file_util).

-export([to_abs/1]).
-export([to_abs/2]).


-include("tts.hrl").


to_abs(FileName) ->
    to_abs(FileName,?CONFIG(config_path)).

to_abs(FileName,BaseDirectory) ->
    to_abs(is_absolute(FileName), FileName, BaseDirectory).

to_abs(true,FileName,_BaseDirectory) ->
    FileName;
to_abs(false,FileName,BaseDirectory) ->
    filename:join(BaseDirectory,FileName).

is_absolute(FileName) when is_binary(FileName) ->
   binary:part(FileName,{0,1}) == <<"/">>;
is_absolute(["/"| _])  ->
    true;
is_absolute([_|_])  ->
    false.
