-module(tts_services).

-export([get_list_for_user/1]).
-export([get_credential_list/1]).
-export([request_credential/4]).
-export([revoke_credential/1]).
-export([security_incident/1]).


get_list_for_user(_UserId) ->
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
    {ok,[]}.

request_credential(ServiceId,UserId,Token,Params) ->
    request_if_allowed(is_allowed(UserId, ServiceId, Token),ServiceId,UserId,Token,Params).

revoke_credential(_CredentialId) ->
    ok.
    
security_incident(_CredentialId) ->
    ok.

request_if_allowed(false, _ServiceId, _UserId, _Token, _Params) ->
    {error, not_allowed};
request_if_allowed(true, _ServiceId, _UserId, _Token, _Params) ->
    {ok, #{}}.

is_allowed(_UserId, _ServiceId, _Token) ->
    true.
