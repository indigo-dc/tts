-module(tts_credential).

-export([get_list/1,
         request/4]).

get_list(_UserId) ->
    {ok, []}.

request(ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    tts_cred_worker:request(ServiceId,UserInfo,Token,Params,Pid).

