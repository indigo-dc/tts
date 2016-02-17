-module(tts_services).

-export([get_service_list/1]).
-export([get_credential_list/1]).
-export([request_credential/3]).
-export([request_credential/4]).
-export([revoke_credential/1]).
-export([security_incident/1]).


get_service_list(_UserId) ->
    SshService = #{ id => <<"service1">>,
                    description => <<"a simple ssh service">>,
                    host => <<"tts.data.kit.edu">>,
                    type => <<"ssh">>
                  },
    S3Service = #{ id => <<"service2">>,
                    description => <<"a simple S3 service">>,
                    host => <<"tts.data.kit.edu">>,
                    type => <<"S3">>
                  },
    ServiceList = [SshService, S3Service],
    {ok,ServiceList}.


get_credential_list(_UserId) ->
    % TODO: implement, should be related to tts_data
    {ok,[]}.

request_credential(ServiceId,UserId,Token) ->
    request_credential(ServiceId,UserId,Token,[]).

request_credential(ServiceId,UserId,Token,Params) ->
    request_if_allowed(is_allowed(UserId, ServiceId, Token),ServiceId,UserId,Token,Params).

revoke_credential(_CredentialId) ->
    ok.
    
security_incident(_CredentialId) ->
    ok.

request_if_allowed(false, _ServiceId, _UserInfo, _Token, _Params) ->
    {error, not_allowed};
request_if_allowed(true, ServiceId, UserInfo, Token, Params) ->
    {ok, Pid} = tts_service_sup:start_worker(),
    tts_service_worker:perform_request(ServiceId,UserInfo,Token,Params,Pid).

is_allowed(_UserId, _ServiceId, _Token) ->
    true.
