# WaTTS - REST Api
WaTTS offers a REST interface to perform all the actions possible via the web-interface.
Actually the web interface uses the REST API.

The WaTTS api includes the version in the path, which is mandatory. WaTTS supports the versions
`v1` and `v2`. This document will describe the protocol version `v2`. The protocol `v1` is
deprecated and should only be used by legacy applications needing the same results as with TTS v0.4.

## List Provider
Retrieving the list of supported OpenId Connect provider is done by
performing a GET request for the `/api/v2/oidcp` path.
```
GET /api/v2/oidcp HTTP/1.1
Host: watts.example.com
User-Agent: curl/7.47.0
Accept: */*
Authorization: Bearer
```
and the reply is (with line breaking for nicer display):
```
{
  "openid_provider_list": [
    {
      "desc": "INDIGO Datacloud Identity and Access Management (IAM)",
      "id": "iam",
      "issuer": "https://iam-test.indigo-datacloud.eu/",
      "ready": true
    }
  ]
}
```
The response is a json object, and includes one key `openid_provider_list` which has
the list of OpenId Providers as a value.  Each Object in the list is one OpenId
Provider, the `id` is the internal id for that OpenId Provider in WaTTS, the
`issuer` is the official issuer returned by the provider. A readable description is
in the `desc` field and `ready` indicates wether the provider is ready to be used.

## List Services
To get the list of services for a user a GET call against the `/api/v2/service`
path.  Authorization is needed for this request please refer to `Authorization
Header` for details.

An example request is:
```
GET /api/v2/service HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://iam-test.indigo-datacloud.eu/
```
And the response is:
```
{
  "service_list": [
    {
      "authorized": true,
      "authz_tooltip": "Your authorisation is insufficient for this service. This may be due to missing group membership or due to a too low Level of Assurance (LoA) (Yes, we already support that kind of stuff ;D)",
      "cred_count": 0,
      "cred_limit": 1,
      "description": "Simple Info Service",
      "enabled": true,
      "id": "info",
      "limit_reached": false,
      "params": [
        []
      ]
    }
  ]
}
```
Again, it is a json object with one key, `service_list`, which holds the list of services as value.
Each entry is a service object, described by its fields.
Important for requesting a credential is the `id`.

## List Credentials
Retrieving the list of credentials currently owned by the user is done by performing a
GET request at the `/api/v2/credenial` path.
Authorization is needed for this request, please refer to `Authorization Header` for details.

An example request is:
```
GET /api/v2/credential HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
The corresponding response might be:
```
{
  "credential_list": [
    {
      "cred_id": "ab23df59-34fb-4394-a109-76cf46beaa66",
      "ctime": "Mon, 30 Jan 2017 14:12:21 GMT",
      "interface": "Web App",
      "service_id": "info"
    }
  ]
}
```
The response consists of a json object with one key, `credential_list`. Its
value is the list of credentials, where each element is again a json object.  For
revoking a credential, only the id of the credential, `cred_id`,  is needed.

## Request Credential
The creation of the credential is triggered by a POST request to
`/api/v2/credential`, with the `service_id` as value in a json object.
Authorization is needed for this request, please refer to `Authorization Header` for details.
It is important to include the 'Content-Type' header with the value 'application/json', else
the request will fail with a 'BAD REQUEST'.

The post data is a simple json object of the form `{"service_id":"<id of service>"}`.

An example would be:
```
POST /api/v2/credential HTTP/1.1
Host: localhost
Accept: */*
Content-Type: application/json
Content-Length: 24

{"service_id":"info"}
```
and the corresponding reply of the TTS is:
```
HTTP/1.1 303 See Other
content-length: 0
content-type: application/json
location: /api/v2/credential_data/rwwwNYX-LaBL0dvZ7wq44g
```
This is a redirection to a location where the credential data is actually available.
Following the redirection, the credential data by the TTS are returned:
```
GET /api/v2/credential_data/rwwwNYX-LaBL0dvZ7wq44g HTTP/1.1
Host: localhost
Accept: */*
```
And the credential is:
```
HTTP/1.1 200 OK
content-type: application/json

{
  "credential": {
    "entries": [
      {
        "name": "WaTTS version",
        "type": "text",
        "value": "1.0.0"
      },
      {
        "name": "WaTTS userid",
        "type": "text",
        "value": "eyJpc3N1ZXIiOiJodHRwczpcL1wvaWFtLXRlc3QuaW5kaWdvLWRhdGFjbG91ZC5ldVwvIiwic3ViamVjdCI6ImZjMTI3MTU2LTExODktNGU1Mi1iNTQyLTUyYzViZTdkMTQ3YiJ9"
      },
      {
        "name": "WaTTS userid (decoded)",
        "type": "text",
        "value": "{\"issuer\":\"https:\\/\\/iam-test.indigo-datacloud.eu\\/\",\"subject\":\"fc127156-1189-4e52-b542-52c5be7d147b\"}"
      },
      {
        "name": "family_name",
        "type": "text",
        "value": "Wegh"
      },
      {
        "name": "Name",
        "type": "text",
        "value": "Bas Wegh"
      },
      {
        "name": "external_authn",
        "type": "text",
        "value": "#{iss => <<\"https://accounts.google.com\">>,\n  sub => <<\"109538112780676045413\">>,\n  type => <<\"oidc\">>}"
      },
      {
        "name": "Issuer",
        "type": "text",
        "value": "https://iam-test.indigo-datacloud.eu/"
      },
      {
        "name": "Gender",
        "type": "text",
        "value": "M"
      },
      {
        "name": "preferred_username",
        "type": "text",
        "value": "bwegh"
      },
      {
        "name": "updated_at",
        "type": "text",
        "value": "Wed Aug 31 09:06:43 UTC 2016"
      },
      {
        "name": "given_name",
        "type": "text",
        "value": "Bas"
      },
      {
        "name": "Groups",
        "type": "textarea",
        "value": "[<<\"Users\">>,<<\"Developers\">>]"
      },
      {
        "name": "organisation_name",
        "type": "text",
        "value": "indigo-dc"
      },
      {
        "name": "Subject",
        "type": "text",
        "value": "fc127156-1189-4e52-b542-52c5be7d147b"
      },
      {
        "name": "json object",
        "type": "textarea",
        "value": "{\n    \"action\": \"request\",\n    \"conf_params\": {},\n    \"cred_state\": \"undefined\",\n    \"params\": {},\n    \"user_info\": {\n        \"external_authn\": {\n            \"iss\": \"https://accounts.google.com\",\n            \"sub\": \"109538112780676045413\",\n            \"type\": \"oidc\"\n        },\n        \"family_name\": \"Wegh\",\n        \"gender\": \"M\",\n        \"given_name\": \"Bas\",\n        \"groups\": [\n            \"Users\",\n            \"Developers\"\n        ],\n        \"iss\": \"https://iam-test.indigo-datacloud.eu/\",\n        \"name\": \"Bas Wegh\",\n        \"organisation_name\": \"indigo-dc\",\n        \"preferred_username\": \"bwegh\",\n        \"sub\": \"fc127156-1189-4e52-b542-52c5be7d147b\",\n        \"updated_at\": \"Wed Aug 31 09:06:43 UTC 2016\"\n    },\n    \"watts_userid\": \"eyJpc3N1ZXIiOiJodHRwczpcL1wvaWFtLXRlc3QuaW5kaWdvLWRhdGFjbG91ZC5ldVwvIiwic3ViamVjdCI6ImZjMTI3MTU2LTExODktNGU1Mi1iNTQyLTUyYzViZTdkMTQ3YiJ9\",\n    \"watts_version\": \"1.0.0-rc7\"\n}"
      }
    ],
    "id": "9b579b68-ba01-44fb-9f00-576a055e12b1"
  },
  "result": "ok"
}
```

## Revoke Credential
To revoke a credential, a DELETE request for that credential is needed. The path of a credential is
`/api/v2/credential/<credential id>`.
Authorization is needed for this request, please refer to `Authorization Header` for details.

A revocation of a credential might look like:
```
DELETE /api/credential/qO10bfakPev2sbW5NWJuCdFKhzG4FmqV HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://accounts.google.com
```
And the result would be a status response.
```
{
  "result": "ok"
}
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
