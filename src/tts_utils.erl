-module(tts_utils).
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

-export([random_string/1]).


-spec random_string(Lenght :: non_neg_integer()) -> binary().
random_string(Length) ->
    Random = try crypto:strong_rand_bytes(Length) of
                 Data -> Data
             catch
                 low_entropy ->
                     timer:sleep(100),
                     random_string(Length)
             end,
    base64url:encode(Random).
