-module(tts_services).

-export([get_list_for_user/1]).
%% -export([request_credential/1]).
%% -export([revoke_credential/1]).
%% -export([security_incident/1]).


get_list_for_user(_Uid) ->
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
