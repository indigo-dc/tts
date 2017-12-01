# OpenId Connect Provider
## Introduction
To provide a login mechanism for the user, at least one OpenId Connect Provider
is needed.

WaTTS needs to be registered as a client with OpenId Connect Provider. For this,
you need to perform the registration process at the Provider of your choice. The
registration process depends heavily on the Provider and is out of the scope of this
documentation, if you are unsure you can ask the provider.

During the registration, some information needs to be provided.
The redirect *uri* is created from three settings:

- `ssl`: http:// (false, default) or https:// (true)
- `hostname`: localhost (default)
- `port`: 8080 (default)
- fix path: /oidc

For the default settings this results in the redirect uri:
http://localhost:8080/oidc.

The redirect uri for the settings 'SSL = true', 'Port = 443', 'HostName=tts.example.com'
would be https://tts.example.com/oidc (the port is not added as it is the default
port for https, it would be the same for port 80 on SSL = false).

If you are unsure, just start the WaTTS and check the logs. During the start of WaTTS, it prints
some messages starting with `Init:`, one of them is `Init: using local endpoint ....`
telling you which uri to use.

WaTTS uses the 'code-auth-flow' and is a 'web-application'.

#### General Settings
WaTTS verifies the complete SSL chain if https is used and is very strict to ensure the
integrity for the user using WaTTS.

To be able to verify a remote certificate WaTTS needs to know where the supported certification
authority certificates are stored. The second information WaTTS needs is the allowed depth of
CA and intermediate CAs to have before reaching the server certificate.


| Key | Description | Datatype | Default |
| :---: | --- | :---: | :---: |
| oidc.cacertfile | The file containing all trusted CAs | file | none |
| oidc.cert_depth | The number of intermediate CAs allowd | integer | 1 |
| oidc.cache_duration | The time, in seconds, http request to userinfo/tokeninfo are allowed to get cacheed. The purpose is to keep high load from the OpenId Connect Provider. Yet this also means that there is a 90 second window in which a user might still be allowed to perform actions although he has been blocked at the IdP | integer, 'none' | 90 |
| oidc.cache_clean | The amount of time (in seconds) to wait before cleaning up the cache | integer | 300 |
| oidc.request_timeout | The number of seconds an http request is allowed to take | integer | 300 |
| oidc.use_cookie | If cookies should be used to identify the user during login | boolean | true |
| oidc.check_user_agent| Wether the user agent identifier should be verified  | boolean | true |
| oidc.check_peer_ip | Should the remote IP be checked when logging in the user | boolean | true |

## Example
```
# this should be the setting on debian based systems if you want to trust the default bundle
oidc.cacertfile = /etc/ssl/certs/ca-certificates.crt
# this should be the setting on centos systems if you want to trust the default bundle
oidc.cacertfile = /etc/ssl/certs/ca-bundle.crt

# on both systems you can cange the allowed depth by
oidc.cert_depth = 5
```


## Provider Settings

| Key | Description | Datatype |
| :---: | --- | :---: |
| description | A description of the Provider, shown at the login Screen | string |
| client_id | The client id received at the registration | string |
| client_secret | The client secret received at the registration | string |
| config_endpoint | The configuration endpoint of the provider, ensure you are using ssl | url |
| request_scopes | the scopes to request | comma separated list |

Each setting is prefixed with 'openid.`id`.' where `id` must be replaced by the id
you want to give to the provider. The `id` MUST not be 'any' or start with 'rsp'.
All settings are mandatory.

## Example
An example for the IAM OpenId Connect Provider, setting its id to `iam`:
```
openid.iam.description = INDIGO Datacloud Identity and Access Management (IAM)
openid.iam.client_id = <insert the client id>
opemid.iam.client_secret =  <insert the client secret>
openid.iam.config_endpoint = https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration
openid.iam.request_scopes = openid, profile
```
