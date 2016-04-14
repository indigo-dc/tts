-module(tts_file_util).

-export([to_abs/1]).
-export([to_abs/2]).


-include("tts.hrl").


to_abs(FileName) ->
    to_abs(FileName, ?CONFIG(config_path)).

to_abs(FileName, BaseDirectory) ->
    filename:join(BaseDirectory, FileName).
