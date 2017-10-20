-module(watts_file_util).
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

-export([read_pem_entries/1]).
-export([to_abs/1]).
-export([to_abs/2]).


-include("watts.hrl").

%% @doc convert a maybe relative path to an absolute path
-spec to_abs(FileName :: string() | binary()) ->  string() | binary().
to_abs(FileName) ->
    combine_or_home(FileName, undefined).

%% @doc convert a maybe relative path to an absolute path, Base beeing the root.
-spec to_abs(Path :: string() | binary(),
                      BasePath :: string() | binary() | undefined)
            ->  string() | binary().
to_abs(FileName, BaseDirectory) ->
    combine_or_home(FileName, BaseDirectory).

%% @doc check if the path starts with ~ and use home as root then.
-spec combine_or_home(Path :: string() | binary(),
                      BasePath :: string() | binary() | undefined)
                     ->  string() | binary().
combine_or_home([ $~, $/  | Relative ], _BaseDir) ->
    convert_home(Relative);
combine_or_home(<< $~:8, $/:8, Relative/binary >>, _BaseDir) ->
    convert_home(Relative);
combine_or_home(NonHome, undefined) ->
    combine_or_home(NonHome, ?CONFIG(config_path, ""));
combine_or_home(NonHome, BaseDir) ->
    filename:join(BaseDir, NonHome).

%% @doc append the path to the home dir.
-spec convert_home(RelativePath :: string() | binary()) -> string() | binary().
convert_home(Relative) ->
    {ok, [[Home]]} =  init:get_argument(home),
    filename:join(Home, Relative).

%% @doc helper function to read pem encoded files
-spec read_pem_entries(Path :: binary()) -> [tuple()].
read_pem_entries(Path) ->
    extract_pem(file:read_file(Path), Path).

%% @doc helper function to decode pem entries
-spec extract_pem({ok, binary()} | any(), binary()) -> [tuple()].
extract_pem({ok, PemBin}, _) ->
    public_key:pem_decode(PemBin);
extract_pem(Error, Path) ->
    lager:error("Init: error reading file ~p: ~p", [Path, Error]),
    [].
