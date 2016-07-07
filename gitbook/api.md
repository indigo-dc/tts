# Token Translation Service Client (TTSc) - REST Api
The Token Translation Service also offers a REST inteface to perform all the actions possible via the 
web-interface.
## Token Translation Service Client (TTSc)
The TTSc is a command line client for the Token Translation Service. Authorization is based on an access token provided as a bearer token with the request. 

### List all OpenId Provider (lsprov)
The `lsprov` command lists all OpenId Provider a TTS instance supports. The call only needs one additional parameter that is the host to connect to:
```
$ ttsc lsprov localhost:8080
"B4A_HsU" "https://accounts.google.com"
"HdPwOcc" "https://iam-test.indigo-datacloud.eu/"
```
In the example above the TTS at localhost on port 8080 will be asked to list all the OpenId Connect Provider it supports. In this example the TTS supports two OpenId Connect provider, accounts.google.com and iam-test.indigo-datacloud.eu. In front of the issuer URLs are the randomly generated ids, which are used by the TTS to refer to the provider.


### List all service for a user (lsserv)
The `lsserv` command lists all the services the TTS supports for the authorized user. The parameter needed are the host of the TTS instance, the access token to authorize and the issuer of the token, so the TTS can verify the token at the issuer.
```
$ ttsc lsserv localhost:8080 ya29.[...]jksb https://accounts.google.com
"opennebula" "opennebula" "oneserver.com" "2633"
"ssh" "ssh" "localhost" "22"
```
In this example the token has been shotened as they are usully pretty long. Instead of the URL of the issuer, *https://accounts.google.com* in this case, also the id *B4A_HsU* could be used.

The result is a list of the services the user can request credentials for. Each line representing one service, printing out the id, the type, the host and the port of the service. For requesting a credential one needs the first column, the id.

### Listing all credentials (lscred)
The `lscred` command lists all currently requested credentials. For this, as with lsserv, the user needs to be authorized by an access token and also providing the issuer so the TTS can verify the access token.
```
$ ttsc lscred localhost:8080 ya29.[...]jksb https://accounts.google.com
{"cred_id":"qO10bfakPev2sbW5NWJuCdFKhzG4FmqV","cred_state":"TTS_CW0MSwY5qBZBPnn4JKpadDqCldrwdia8","ctime":1467877711,"interface":"web interface","service_id":"ssh"}
{"cred_id":"2GkzHmpkgVwywckpeIiP-5dpEes0iESe","cred_state":"TTS_nI4XxZPkLCLaQuTCkroAQfGaRVkXCcvY","ctime":1467878940,"interface":"REST interface","service_id":"ssh_tts"}
```
The output is one json object per credential. 
- `cred_id` is the internal identifier of the credential within TTS.
- `cred_state` is the state returned by the plugin and stored at the TTS to revoke the credential. 
- `ctime` is the creation time
- `interface` indicates at which interface the credential was created either the web or the REST inteface 
- `service_id` is the id of the service and links to the service list (see lsserv).

### Requesting a credential (request)
The `request` command is used to request a credential from the Token Translation Service. To create a credential the TTS needs the service_id as well as access token and the issuer as parameter.

```
$ ttsc request localhost:8080 ssh ya29.[...]jksb https://accounts.google.com
[
{"name":"id","type":"text","value":"2GkzHmpkgVwywckpeIiP-5dpEes0iESe"},
{"name":"Username","type":"text","value":"indigo_2001"},
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
The output is a list of tuples, each tuple representing a part of the credential, most of them 
depend on the plugin, as they are specific to the service, yet there is one entry that is always included: 
- `id`: the value is the internal id in the TTS, see also lscred, which also shows this credential

### Revoking a credential (revoke)
Revoking is very similar to requesting, yet instead of providing the service for which to request a credential the credential id is provided.
So the list of parameter are: Credential id, access token and issuer.

```
$ ttsc request localhost:8080 2GkzHmpkgVwywckpeIiP-5dpEes0iESe ya29.[...]jksb https://accounts.google.com
```
Only in case of an error you get an output, else the credential is revoked. 
Checking the list of credentials using the lscred command shows that only one credential is left:
```
$ ttsc lscred localhost:8080 ya29.[...]jksb https://accounts.google.com
{"cred_id":"qO10bfakPev2sbW5NWJuCdFKhzG4FmqV","cred_state":"TTS_CW0MSwY5qBZBPnn4JKpadDqCldrwdia8","ctime":1467877711,"interface":"web interface","service_id":"ssh"}
```

### Plugin Developer HTTP support 
If you need to connect to an TTS instance that does not support SSL you should contact the administrator and ask him to set it up. 
A TTS without SSL MUST NOT run in production as convidential data are transmitted.

For developers of plugins there is the possibility to set the environment variable `HTTP_SCHEME` to `http` to get a non SSL connection.

Further explanations are not added on purpose as this creates a HUGE security hole, you are warned.

## REST Api
### List Providers 
### List Services 
### List Credentials 
### Request Credential 
### Revoke Credential 
