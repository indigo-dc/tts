-module(tts_credential).

-export([get_list/1,
         request/4]).

get_list(_UserId) ->
    {ok, []}.

request(_ServiceId, _UserInfo, _Token, _Params) ->
    ok.

