-module(tts_credential).

-export([get_list/1,
         request/4]).

get_list(_UserId) ->
    {ok, []}.

request(ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_cred_sup:new_worker(),
    Result = tts_cred_worker:request(ServiceId,UserInfo,Params,Pid),
    handle_request_result(Result,UserInfo,Token).


handle_request_result({ok,#{credential := _Cred, state := _State}},UserInfo,_Token) ->
    %TODO: store the credential and the needed params for revoke/incident 
    ok;
handle_request_result({error,_},_UserInfo,_Token) ->
    ok.

