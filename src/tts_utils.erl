-module(tts_utils).
-export([random_string/1]).


-spec random_string(Lenght :: non_neg_integer()) -> binary().
random_string(Length) ->
    Random = try crypto:strong_rand_bytes(Length) of
                 Data -> Data
             catch
                 low_entropy ->
                     crypto:rand_bytes(Length)
             end,
    base64url:encode(Random).
