# WaTTS - REST Api
This documentation is for developers. If you want to use WaTTS at the command line level, please have a look at [wattson](https://github.com/indigo-dc/wattson).


WaTTS offers a REST interface to perform all the actions possible via the web-interface.
Actually the web interface uses the REST API.

The WaTTS api interface is versioned, the version is part of the path. WaTTS supports the versions
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
```
and the reply body is similar to (with line breaking for nicer display):
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
To get the list of services for a user a GET request against the `/api/v2/service`
path is performed.  Authorization is needed for this request please refer to
`Authorization Header` for details.

An example request is:
```
GET /api/v2/service HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://provider.example.com/
```
And the response is similar to:
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
It is a json object with one key, `service_list`, which holds the list of services as value.
Each entry is a service object, described by its fields.
Important for requesting a credential is the `id`.
 - authorized: wether the user is allowed to use this service
 - authz_tooltip: information on how the user might get the authorization
 - cred_count: the number of credentials for this service
 - cred_limit: how many credentials are allowed
 - description: a textual description of the service
 - enabled: wether this service is actually enabled
 - id: the id of the service, used to perform requests
 - limit_reached: wether the credential limit is reached
 - params: parameter sets, allowed to pass parameter to requests

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
X-OpenId-Connect-Issuer: https://provider.example.com
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

 - cred_id: the id of the credential
 - ctime: the creation time
 - interface: wich interface was used, either 'Web App' or 'REST'
 - service_id: the id of the service it was issued for


## Request Credential
The creation of the credential is triggered by a POST request to
`/api/v2/credential`, with the `service_id` as value in a json object.
Authorization is needed for this request, please refer to `Authorization Header` for details.
It is important to include the 'Content-Type' header with the value 'application/json', else
the request will fail with a 'BAD REQUEST'.

### Basic Request
A basic request is a just a simple post of the service id with no parameter.
The post data is then simple json object of the form `{"service_id":"<id of service>"}`.

A service might not support a basic request. The basic request is supported if one
of the following is true:
 - the `params` value of the service is an empty list
 - one of the lists in `params` is an empty list
 - one of the lists in `params` contains only entries with `mandatory` set as `false`

An example would be:
```
POST /api/v2/credential HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://provider.example.com
Content-Type: application/json
Content-Length: 24

{"service_id":"info"}
```


### Advanced Request
an advanced request is a request that adds paramater to a request. Basically it is just
extending the json object of the basic request.
The important thing to note is that the keys need to be the same as specified by the
service.

Services that support parameter have a list of parameter sets given at the `params` key:
```
{
  "service_list": [
    {
      "id": "with_params",
      ....
      "params": [
        [
            [
             { "key":"param_key",
               "name":"Name of Parameter",
               "description":"a long description of the parameter",
               "type":"textarea",
               "mandatory":true}
           ],
           []
       ]
      ]
    }
  ]
}
```
Each parameter set is a list of parameter. A request has to satisfy one set, meaning to give at
least all mandatory fields of that set. The example above has two sets, one empty set and one
set with one mandatory parameter `param_key`.

The meaning of the parameter depends upon the service and should be understood by the description.

To pass parameter to the request an aditional key value pair is added to the json object,
the key being `params` and its value is a json object with each key being the value of the `key` value
of the parameter entry in the set. In the above example it would be `param_key`.

The advanced post body for this service might look like:
```
{ "service_id":"with_params",
  "params": {
              "param_key":"my passed parameter value"
            }
}
```



### Credential Response
The response of WaTTS is a redirection:
```
HTTP/1.1 303 See Other
content-length: 0
content-type: application/json
location: /api/v2/credential_data/rwwwNYX-LaBL0dvZ7wq44g
```
This is a redirection to the location of the actual credential data.
Following the redirection, the credential data are returned by WaTTS:
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
        "value": "eyJpZXIiYiJ9"
      },
      {
        "name": "WaTTS userid (decoded)",
        "type": "text",
        "value": "{\"issuer\":\"https://provider",\"subject\":\"sub\"}"
      },
      {
        "name": "family_name",
        "type": "text",
        "value": "Mustermann"
      },
      {
        "name": "Name",
        "type": "text",
        "value": "Max Mustermann"
      },
      {
        "name": "Issuer",
        "type": "text",
        "value": "https://provider"
      },
      {
        "name": "Gender",
        "type": "text",
        "value": "M"
      },
      {
        "name": "preferred_username",
        "type": "text",
        "value": "mm"
      },
      {
        "name": "updated_at",
        "type": "text",
        "value": "Wed Aug 31 09:06:43 UTC 2016"
      },
      {
        "name": "given_name",
        "type": "text",
        "value": "Max"
      },
      {
        "name": "Groups",
        "type": "textarea",
        "value": "[<<\"Users\">>]"
      },
      {
        "name": "Subject",
        "type": "text",
        "value": "sub"
      }
    ],
    "id": "9b579b68-ba01-44fb-9f00-576a055e12b1"
  },
  "result": "ok"
}
```
each entry consists of
 - name: the name of the value
 - type: the datatype
 - value: the actual value
the meaning of those entries depend on the service/plugin being used.

## Revoke Credential
To revoke a credential, a DELETE request for that credential is needed. The path of a credential is
`/api/v2/credential/<credential id>`.
Authorization is needed for this request, please refer to `Authorization Header` for details.

A revocation of a credential might look like:
```
DELETE /api/v2/credential/qO10bfakPev2sbW5NWJuCdFKhzG4FmqV HTTP/1.1
Host: localhost
Accept: */*
Authorization: Bearer ya29.[...]MUchM
X-OpenId-Connect-Issuer: https://provider.example.com
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
include the issuer URL of the provider:
```
X-OpenId-Connect-Issuer: https://provider.example.com
```
