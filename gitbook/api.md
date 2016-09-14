# Token Translation Service Client (TTSc) - REST Api
The Token Translation Service also offers a REST interface to perform all the
actions possible via the web-interface.
## Token Translation Service Client (TTSc)
The TTSc is a command line client for the Token Translation Service.
Authorization is based on an access token provided as a bearer token with the
request.

### List all OpenId Provider (lsprov)
The `lsprov` command lists all the OpenId Providers a TTS instance supports. The call
only needs one additional parameter, which is the host used for connection:
```
$ ttsc lsprov localhost:8080
"B4A_HsU" "https://accounts.google.com"
"HdPwOcc" "https://iam-test.indigo-datacloud.eu/"
```
In the example above, the TTS at localhost on port 8080 will be asked to list all
the OpenId Connect Providers it supports. For this example, the TTS supports two
OpenId Connect providers, accounts.google.com and iam-test.indigo-datacloud.eu.
In front of the issuer URLs are the randomly generated ids, which are used by
the TTS to refer to the provider.


### List all service for a user (lsserv)
The `lsserv` command lists all the services the TTS supports for the authorized
user. The needed parameters are the host of the TTS instance, the access token to
authorize, and the issuer of the token, so the TTS can verify the token at the
issuer.
```
$ ttsc lsserv localhost:8080 ya29.[...]jksb https://accounts.google.com
"opennebula" "opennebula" "oneserver.com" "2633"
"ssh" "ssh" "localhost" "22"
```
In this example the token has been shortened as they are usually pretty long.
Instead of the URL of the issuer, *https://accounts.google.com* in this case,
also the id *B4A_HsU* can be used.

The result is a list of the services for which the user can request credentials.
Each line represents one service, listing the id, the type, the host, and
the port of the service. For requesting a credential, one needs the first column,
the id.

### Listing all credentials (lscred)
The `lscred` command lists all currently requested credentials. For this, as
with `lsserv`, the user needs to be authorized by an access token and also needs
to provide the issuer, so the TTS can verify the access token.
```
$ ttsc lscred localhost:8080 ya29.[...]jksb https://accounts.google.com
{"cred_id":"qO10bfakPev2sbW5NWJuCdFKhzG4FmqV","cred_state":"TTS_CW0MSwY5qBZBPnn4JKpadDqCldrwdia8","ctime":1467877711,"interface":"web interface","service_id":"ssh"}
{"cred_id":"2GkzHmpkgVwywckpeIiP-5dpEes0iESe","cred_state":"TTS_nI4XxZPkLCLaQuTCkroAQfGaRVkXCcvY","ctime":1467878940,"interface":"REST interface","service_id":"ssh_tts"}
```
The output is one json object per credential.
- `cred_id` is the internal identifier of the credential within TTS.
- `cred_state` is the state returned by the plugin and stored at the TTS to
  revoke the credential.
- `ctime` is the creation time
- `interface` indicates with which interface the credential was created, it is either a
  web or the REST interface
- `service_id` is the id of the service and links to the service list (see `lsserv`).

### Requesting a credential (request)
The `request` command is used to request a credential from the Token Translation
Service. To create a credential, the TTS needs the service_id as well as an access
token and the issuer as a parameter.

```
$ ttsc request localhost:8080 ssh ya29.[...]jksb https://accounts.google.com
[
{"name":"id","type":"text","value":"2GkzHmpkgVwywckpeIiP-5dpEes0iESe"},
{"name":"Username","type":"text","value":"tts_user"},
{"cols":"64","name":"Private Key","rows":"30","type":"textfile",
"value":["-----BEGIN RSA PRIVATE KEY-----\n
Proc-Type: 4,ENCRYPTED\n
DEK-Info: AES-128-CBC,9232C34FB60C8FCA22E2F8B7832542E9\n
\n
jq6DLWAT5p+joZ+3PIXjNL4qHViWxVihQnmMMZolLkXMvWPUGJyXdPp4ZnsuKfdO\n
thRcmzkG0+wyx/cgXkPTQcbMB9A6R7rxRk3ylsBOUwbmeD64s+P46QDLwOQsO8In\n
N0ujyT7pCuUEem0kiAzER6OKNjkzN7d1U98TPNKRI214tUYqSRkxZGp+bSzvglbN\n
EfkY5vcmI4LGkcSn0JfoGbohanc46IDn241xCJOtCKBFs4l+lk+kE+19yPObpULt\n
9aN7JKV5OO0yNkt52YtEDdM9S0PpkSCZHvC9DkKFn1U8/bnLcaCONUZZt+PZcMQ4\n
LNTg/Dz+xt7dBVRP7ZfN/+39+8sDSk59VG9D3xrogk89HYkkobqyxL3IKPShExwY\n
PDnfapjCU5OFmIBRk380Lgv68PJBMk3U2/+skMjLmyxDr9x67LPQ4VO9SBHNXJF3\n
FJcbu7Nj+5NLNON5eS+JEzfJfQ+vV9DIBveDGGkA6puv0+sO9CzpMzuubWFUXzwy\n
fSoHzfLJbUldMqAb3AfDdcvyuPDKOd4FKSOEuwx6IlH5VxxJmqXAKQwou1xZ9yxP\n
ebjnyEl5AhL8njN88bAoe0Tq1k5c6/P+mi/+OHpAh+I9eC/PYlCSTgqm65+9kuGX\n
tWsSYVHf6XDExpmRvNUx+n5Ld6CllJdFss1JL0r3QoDB1ZN+LhAi6nRrHBZG/8yp\n
proSstlFhP+otgqFkVIE86vZsyh6QhKiOEOiIWQLDCuSzxfI0/qUh+9CC2kKN+7S\n
c/i3m3DQS0GWEnq1vWjdroRgorE4AFoJQy2EHpSZn9vmGYgvVdX5wxAqWw2VXSpt\n
e+FqFeV35PISB5IxpEhmozhtyvAX21Chox/G6ZlNUFXdC/IGbm42NvtufV/vNlEz\n
oeZHyaX+n0PzEpPFdRruQx3rnPxnjkNLh6XEAU8AbRlCag1qPGan6VQnVImeIVPZ\n
aW/oiX9bxn84mxK4U4NLTBY8oLr175AfMgFsfSvOqYf4dK1fsu1VMt8Y/d7Uj8MC\n
OSjj+YA5NWN3HKYLhgGh0jmHRkxTlfYRlCBGFZXe7kPJELUo1dBeTb8D5j6GjY0F\n
ZRB62oxCOa8t8ZQqKzyc4K8jDveg8zOUvvJ3s1IEkYL2Gr7o8c65IKPEPZ6hDiFw\n
kvG5zqG4BcLA2SwCnT4Z0QAs+qC95thSFtA31NMJcX92MMqyRdrILcR5Fl0rqMfJ\n
f9unE10TmEqy5X1BAeVCznmdOAGv+vmJ3XCD9KHdUCH29ZM21E1uPYbG6hNW92jL\n
6CB5Zb+O0lbhvessQWvXrihf0q8p5JetzQE47+kt7AJPNniMJkn8AnXpHD4Ed1fU\n
N7YNrEsfV8KM3c94IATYileXUEGqwxSsXceyWZGmF3wfO5QEP4bTeZu0LBQ6qHUJ\n
BMUyglhEai45ulGzkVM56xEqRlteLVI4apWvwdH/L89Cgf9gnxTSyWvSuR5KtPiA\n
kHqXV0zx3jMxttLTvrMvyM6L8SlKOTNaqQ1wCg4DZLopuZ8ilQzaTBez7rlhcXMm\n
AOhJBqAofH9BFowVAeUQcWpgP//vwsmyzI7biZxX/DTuojOqTLXopc7hFtluKpPY\n
-----END RSA PRIVATE KEY-----\n"]},
{"name":"Passphrase (for Private Key)","type":"text","value":"qU1r45cgUa61f1Bn"}
]
```
The output is a list of tuples, with each tuple representing a part of the
credential. Most of them depend on the plugin, as they are specific to the
service, yet there is one entry which is always included:
- `id`: the value is the internal id in the TTS, see also `lscred`, which also
  shows this credential

### Revoking a credential (revoke)
Revoking is very similar to requesting, yet instead of providing the service for
which to request a credential, the credential id is provided.  So the list of
parameter are: Credential id, access token, and issuer.

```
$ ttsc request localhost:8080 2GkzHmpkgVwywckpeIiP-5dpEes0iESe ya29.[...]jksb https://accounts.google.com
```
Only in case of an error you get an output, otherwise the credential is revoked.
Checking the list of credentials using the `lscred` command shows that only one
credential is left:
```
$ ttsc lscred localhost:8080 ya29.[...]jksb https://accounts.google.com
{"cred_id":"qO10bfakPev2sbW5NWJuCdFKhzG4FmqV","cred_state":"TTS_CW0MSwY5qBZBPnn4JKpadDqCldrwdia8","ctime":1467877711,"interface":"web interface","service_id":"ssh"}
```

### Plugin Developer HTTP support
If you need to connect to an TTS instance that does not support SSL, you should
contact the administrator and ask him to set it up.  A TTS without SSL MUST NOT
run in production as confidential data are transmitted.

For developers of plugins there is the possibility to set the environment
variable `HTTP_SCHEME` to `http` to get a non SSL connection.

Further explanations are omitted on purpose as this creates a HUGE security
hole, you are warned.

## REST API
The Token Translation Service client (TTSc) from the previous chapter uses the
REST interface to perform all the described actions. In this chapter, the
pure REST calls will be described, this is of use only if an application needs
to communicate directly with the TTS.

### List Provider
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

### List Services
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

### List Credentials
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

### Request Credential
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

### Revoke Credential
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

### Authorization Header
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
