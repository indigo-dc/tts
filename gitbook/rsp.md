# Relying Service Provider (RSP) Guide
## Introduction
An RSP is a service that relys on WaTTS to either perform tasks or to authenticate users
for it, or both.

Any RSP that wants to rely on WaTTS needs to be configured as described in the configuration guide.
The two important steps are:
- enable RSP support with `enable_rsp = true`
- configure the RSP according to the 'Relying Service Provider' configuration

## How it works
WaTTS enables an rsp endpoint at `/rsp` at wich an RSP can redirect its users.
The full path to redirect to is `https://watts-host/rsp/<jwt>`.

The `<jwt>` is a JSON Web Token (JWT) generated and signed by the RSP.
WaTTS will verify the JWT and perform the requested actions, if allowed.

On error or success the user gets redirected back to the RSP.


## What an RSP needs to implement
### A JWK endpoint
WaTTS needs an endpoint to fetch the public keys used to sign the JWT.
The json format of the plublic keys file must be according to
[RFC7517](https://tools.ietf.org/html/rfc7517). The following is an example:
```
{"keys":
       [
         {"kty":"RSA",
          "use":"sig",
          "n": "0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx
     4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMs
     tn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2
     QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbI
     SD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqb
     w0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw",
          "e":"AQAB",
          "alg":"RS256",
          "kid":"2011-04-29"}
       ]
     }
```
WaTTS expects the following keys to be present:
 - kty of "RSA"
 - use of "sig"
 - alg of "RS256"
 - kid which must also be set in the JWT sent by the RSP
 - n (part of the public key)
 - e (part of the public key)

### Generation of JWTs
The RSP must implement or use a JWT library signing the JWTs with the private part of the keys
mentioned above.

The following claims in the JWT are supported:

| Key | Description | Datatype | Mandatory |
| :---: | --- | :---: | :---: |
| iss | The Id of the RSP in the configuration file at WaTTS | string | yes |
| sub | The representation of the user at the RSP | string | yes |
| exp | The expiration time of the jwt [see RFC7519](https://tools.ietf.org/html/rfc7519#section-4.1.4) | integer | yes |
| iat | The time at which the jwt was issued [see RFC7519](https://tools.ietf.org/html/rfc7519#section-4.1.6) | integer | yes |
| watts_service | The Id of the service to trigger at WaTTS | string | yes |
| watts_params | The parameter to pass to the service at WaTTS | JSON object | no |
| watts_provider | The Id of the OpenID Connect provider to use for logging in | string | no |
| success_url | The url to redirect to in case of success, if not set WaTTs will redirect to the referer | string | no |
| failed_url | The url to redirect to in case of failure, if not set WaTTs will redirect to success_url | string | no |
