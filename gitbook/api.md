# Token Translation Service Client (TTSc) - REST Api
The Token Translation Service also offers a REST interface to perform all the
actions possible via the web-interface.


## List Provider
Retrieving the list of supported OpenId Connect provider is done by
performing a GET request for the `/api/oidcp` path.
```
GET /api/oidcp HTTP/1.1
Host: localhost
User-Agent: curl/7.47.0
Accept: */*
Authorization: Bearer
```
and the reply is (with line breaking for nicer display):
```
HTTP/1.1 200 OK
content-type: application/json

{"openid_provider_list":
	[
	{"id":"ExgxscQ","issuer":"https://accounts.google.com"},
	{"id":"GGdaLGU","issuer":"https://iam-test.indigo-datacloud.eu/"}
	]
}
```
So it is a json object, and includes one key `openid_provider_list` which has
the list of OpenId Providers as a value.  Each Object in the list is one OpenId
Provider, the `id` is the internal id for that OpenId Provider in TTS, the
`issuer` is the official issuer returned by the provider.

## List Services
To get the list of services for a user a GET call against the `/api/service`
path.  Authorization is needed for this request please refer to `Authorization
Header` for details.

An example request is:
```
GET /api/service HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
And the response is:
```
HTTP/1.1 200 OK
content-length: 159
content-type: application/json

{"service_list":
	[
	{"host":"tts.data.kit.edu","id":"ssh","port":"22","type":"ssh"}
	]
}
```
Again, it is a json object with one key, `service_list`, which holds the list of services as value.
Each entry is a service object, described by its fields.
Important for requesting a credential is the `id`.

## List Credentials
Retrieving the list of credentials currently owned by the user is done by performing a
GET request at the `/api/credenial` path.
Authorization is needed for this request, please refer to `Authorization Header` for details.

An example request is:
```
GET /api/credential HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
The corresponding response might be:
```
HTTP/1.1 200 OK
content-length: 197
content-type: application/json

{"credential_list":
	[
	{"id":{"cred_id":"qO10mqV","cred_state":"TTS_CWMSia8","ctime":1467877711,"interface":"web interface","service_id":"ssh"}}
	]
}
```
The response consists of a json object with one key, `credential_list`. Its
value is the list of credentials, where each element is again a json object.  For
revoking a credential, only the `id` of the credential is needed.

## Request Credential
The creation of the credential is triggered by a POST request to
`/api/credential`, with the `service_id` as value in a json object.
Authorization is needed for this request, please refer to `Authorization Header` for details.
It is important to include the 'Content-Type' header with the value 'application/json', else
the request will fail with a 'BAD REQUEST'.

The post data is a simple json object of the form `{"service_id":"<id of service>"}`.

An example would be:
```
POST /api/credential HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
Content-Type: application/json
Content-Length: 24

{"service_id":"ssh"}
```
and the corresponding reply of the TTS is:
```
HTTP/1.1 303 See Other
content-length: 0
content-type: application/json
location: /api/v1/credential_data/rwwwNYX-LaBL0dvZ7wq44g
```
This is a redirection to a location where the credential data is actually available.
Following the redirection, the credential data by the TTS are returned:
```
GET /api/v1/credential_data/rwwwNYX-LaBL0dvZ7wq44g HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
And the credential is:
```
HTTP/1.1 200 OK
content-length: 2083
content-type: application/json

[
{"name":"id","type":"text","value":"K1IaHgJt4kYUJk16tCsSEwO9wkBhaObH"},
{"name":"Username","type":"text","value":"tts_user"},
{"name":"Password","type":"text","value":"secret"}
]
```
The result is a list of attributes of the credential, each element is a json
object defining one part of the credential.

The only object that is always present is the object whose `name` is `id`, all
other object depend on the plugin and on the service for which the credential is
requested.

## Revoke Credential
To revoke a credential, a DELETE request for that credential is needed. The path of a credential is
`/api/credential/<credential id>`.
Authorization is needed for this request, please refer to `Authorization Header` for details.

A revocation of a credential might look like:
```
DELETE /api/credential/qO10bfakPev2sbW5NWJuCdFKhzG4FmqV HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
And the result would be a pure 204, as the credential has been successfully deleted.
```
HTTP/1.1 204 No Content
content-length: 0
content-type: application/json
```

## Authorization Header
To get the request authorized, two header fields need to be set.
The first header is the `Authorization` header. Following the OAuth2.0 spec, the
access token will be added into the authorization header as a bearer token:
```
Authorization: Bearer ya29.[...]uMUchM
```

The second header field is the `X-OpenId-Connect-Issuer` header, which must
include either the TTS id of the OpenId Connect provider or its issuer URL:
```
X-OpenId-Connect-Issuer: https://accounts.google.com
```
