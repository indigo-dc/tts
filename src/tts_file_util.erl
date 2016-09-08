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


to_abs(FileName) ->
    combine_or_home(FileName, undefined).

to_abs(FileName, BaseDirectory) ->
    combine_or_home(FileName, BaseDirectory).

combine_or_home([ $~, $/  | Relative ], _BaseDir) ->
    convert_home(Relative);
combine_or_home(<< $~:8, $/:8, Relative/binary >>, _BaseDir) ->
    convert_home(Relative);
combine_or_home(NonHome, undefined) ->
    combine_or_home(NonHome, ?CONFIG(config_path, ""));
combine_or_home(NonHome, BaseDir) ->
    filename:join(BaseDir, NonHome).

convert_home(Relative) ->
    {ok, [[Home]]} =  init:get_argument(home),
    filename:join(Home, Relative).
