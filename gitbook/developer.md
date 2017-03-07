# Developer Guide
## Plugin Development
To enable a certain type of service for WaTTS, a corresponding *plugin* is
needed.  Sometimes there are even multiple plugins for the same type of service,
e.g. due to different local policies.

The communication between WaTTS and the *plugin* is done by passing one argument
to the script and parsing the `stdout` of the script.

### Plugin Input
The parameter coming from the TTS is a `base64` encoded `json` object.

The decoded `json` object has the following format:
```
{
    "watts_version": "1.0.0",
    "action": "request",
    "conf_params": {},
    "params": {},
    "cred_state": "undefined",
    "user_info": {
        "family_name": "Mustermann",
        "gender": "male",
        "given_name": "Max",
        "iss": "https://issuer.example.com",
        "name": "Max Mustermann",
        "sub": "12345678"
    },
    "watts_userid": "base64 encoded json object, containing subject and issuer"
}
```
The items of the object are:

| Key        | Description |
|----|----|
| watts_version | The version of WaTTS, e.g. can be used for a *plugin* to handle newer parameters |
| action | Which action to perform, this can be either *request*, *revoke* or *parameter* |
| conf_params| These are parameters that are set in the configuration file |
| params |  These are parameters entered upon request |
| cred_state | This is only valid when revoking, it is the credential state returned when creating the credential |
| user_info  | The user info contains information about the user gathered using OpenId Connect |
| watts_userid | This is the dynamically generated unique *id* used within WaTTS for the current user. It is also a `base64` encoded `json` object containing *issuer* and *subject* |

### Listing The Supported Parameter
WaTTS requests the supported parameter of the *plugin* by setting the `action` to `parameter`.

The expected result is:
```
{
    "result":"ok",
    "conf_params": [ {'name':'full_access', 'type':'boolean', 'default': False},
                     {'name':'state_prefix', 'type':'string', 'default':'TTS_'},
                     {'name':'override_user_name', 'type':'boolean', 'default':False }
                   ],
    "request_params":
    [
        [
            {'key':'pub_key', 'name':'public key',
            'description':'the public key to upload to the service',
            'type':'textarea', 'mandatory':True}
        ],
        []
    ],
    "version":"the plugin version"
}
```
The result MUST be OK and the version should describe the version of your *plugin* so e.g. errors
can be tracked. All entries are mandatory.

#### Config Parameter (conf_params)
Each config parameter consists of an `json` object with three key-value pairs:
 - *name*: the name of the parameter, MUST consists of only [a-z0-9_]
 - *type*: the datatype of the parameter, currently supported are `boolean` and `string`
 - *default*: the default value to use if not configured, please ensure safe defaults

#### Request Parameter Sets (request_params)
The supported request parameters are grouped into sets. A set is a group of parameters that
belong together logically.
At the web interface, a parameter set is shown in one dialog page, to switch between them
press 'left' or 'right' in the carousel of sets.

The `Advanced` button at the web-interface is enabled if at least one non empty parameter
set exists. The `Request` button is enabled if either the empty set exists (see example above),
or a set exists with no mandatory parameters.

Each entry in a set is described with five key-value pairs:

- *key*: a key used to pass the value, this is also the key to lookup the information when performing a request, MUST consist only of [a-z0-9_].
- *name*: a short nice name, displayed to the user
- *description*: a longer description, also shown to the user
- *type*: the input type to use, currently only supported `textarea`
- *mandatory*: either `True` or `False`, indicates whether this field MUST be filled


### Performing a Request
When the `action` is set to `request`, it means that a user or a service on a
user's behalf wants to create a credential.
The *plugin* should perform any necessary action to create the credential for the user and print
a `json` object on `stdout`.

The expected format of the `json` object is:

```
{
	"result":"ok",
	"credential": [{"name":"some name", "type":"text", "value":"secret"}],
	"state":"state description"
}
```
or in case of an error:

```
{
	"result":"error",
    "user_msg":"Sorry, you are not allowed to use this service",
    "log_msg":"user joe tried to access service and has been denied"
}
```


| Key        | Description                                                                                    |
|------------|------------------------------------------------------------------------------------------------|
| result | The result of the request, currently *ok* and *error* are supported |
| credential | A list of objects, each representing one part of the credentials. This will be shown to the user |
| state      | A state to keep track of the credential; it MUST NOT contain sensitive information              |
| user_msg  | If the result is an error, this message will be shown to the user        |
| log_msg  | If the result is an error, this message will be logged        |


The entries in the credential object (part of the list in 'credential' above) are:

| Key   | Description                                                                                                       |
|-------|-------------------------------------------------------------------------------------------------------------------|
| name  | The name the user will see, e.g. username or password                                                              |
| type  | The type of data, this will determine how it will be displayed. At the moment `text` and `textfile` are supported |
| rows  | The number of rows for the `textarea` in case the type is `textfile`, else ignored                                        |
| cols  | The number of columns for the `textarea` when using `textfile`, else ignored                                          |
| value | The value of the credential part, e.g. the password or the username itself                                         |


### Performing a Revoke
When the `action` is set to `revoke`, the credential identified by the `state` should be invalidated
or removed, depending on the type of service. In the SSH plugin, e.g., the created ssh-key is
removed from the authorized keys.
When performing a request, WaTTS expects the `stdout` to be a `json` object. In case of *revoke*, it may contain
one of the two elements:
```
{
	"result":"ok"
}
```
or in case of an error:

```
{
	"result":"error",
    "user_msg":"Sorry, could not revoke the certificate",
    "log_msg":"internal error when executing command ...."
}
```
| Key    | Description                                                             |
|--------|-------------------------------------------------------------------------|
| result | either *ok* or *error*, representing the result of the *plugin*           |
| user_msg  | If the result is an error, this message will be shown to the user        |
| log_msg  | If the result is an error, this message will be logged        |


Therefore, if only the result key is present in the object, WaTTS assumes that
everything went fine, however, if the error key is present it assumes that
something went bad and does not remove the credential state from its database.
