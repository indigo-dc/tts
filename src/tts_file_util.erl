-module(tts_file_util).
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

-export([to_abs/1]).
-export([to_abs/2]).


-include("tts.hrl").


to_abs([First, Second | Rest] = FileName) ->
    convert_to_abs(First, Second, Rest, FileName);
to_abs(FileName) when is_binary(FileName) ->
    << First:1/binary, Second:1/binary, Rest/binary >> = FileName,
   convert_to_abs(First, Second, Rest, FileName).

convert_to_abs($~, $/, Rest, _FileName) ->
    convert_home(Rest);
convert_to_abs(<<"~">>, <<"/">>, Rest, _FileName) ->
    convert_home(Rest);
convert_to_abs($/, _Second, _Rest, FileName) ->
    FileName;
convert_to_abs(<<"/">>, _Second, _Rest, FileName) ->
    FileName;
convert_to_abs(_First, _Second, _Rest, FileName) ->
    to_abs(FileName, ?CONFIG(config_path)).

convert_home(Relative) ->
    {ok, [[Home]]} =  init:get_argument(home),
    to_abs(Relative, Home).


to_abs(FileName, BaseDirectory) ->
    filename:join(BaseDirectory, FileName).
