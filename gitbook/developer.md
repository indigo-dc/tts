# Developer Guide
## Plugins
To enable a certain type of service for the TTS, a corresponding plugin is
needed.  Sometimes there are even multiple plugins for the same type of service,
e.g. due to different local policies.

### Writing Plugins
The communication between the TTS and the plugin is done by passing one argument
to the script and parsing the stdout of the script.

#### Plugin Input
The parameter coming from the TTS is a base64 encoded json object.

The decoded json object has the following format:
```
{
    "action": "request",
    "conf_params": {},
    "params": {},
    "cred_state": "undefined",
    "tts_userid": "<the generated unique userid used in TTS>",
    "tts_version": "<the tts version>",
    "user_info": {
        "family_name": "Mueller",
        "gender": "male",
        "given_name": "Joe",
        "iss": "https://accounts.google.com",
        "name": "Joe Mueller",
        "profile": "https://plus.google.com/1234",
        "sub": "1234"
    }
}
```
The items of the object are:

| Key        | Description                                                                                         |
|------------|-----------------------------------------------------------------------------------------------------|
| action     | Which action to perform, this can be either 'request', 'revoke' or 'parameter'                                    |
| conf_params| These are parameter that are set in the configuration                                     |
| params |  These are parameter entered at the request                                     |
| cred_state | This is only passed when revoking, it is the credential state returned when creating the credential |
| tts_userid | This is the dynamically generated unique id used within TTS for the current user. It is also a base64 encoded json object containing issuer and subject |
| tts_version | The version of the TTS, this is passed so a plugin can handle api changes |
| user_info  | The user info contains informations gathers about the user using OpenId Connect          |

#### Listing The Supported Parameter
% TODO
#### Performing a Request
When the `action` is set to `request`, it means that the user or a service on the
users behalf wants to create a credential.
The plugin should perform any necessary action to create the credential for the user and print
a json object on stdout.

The expected format of the json object is:
```
{
	"result":"ok",
	"credential": [{"name":"some name", "type":"text", "value":"secret"}],
	"state":"state description"
}
```
or in case of an error
```
{
	"result":"error",
    "user_msg","Sorry, you are not allowed to use this service",
    "log_msg","user joe tried to access service and has been denied"
}
```


| Key        | Description                                                                                    |
|------------|------------------------------------------------------------------------------------------------|
| result | The result of the request, currently 'ok' and 'error' are supported |
| credential | A list of objects, each representing one part of the credentials. This will be shown to the user |
| state      | A state to keep track of the credential; it MUST NOT contain sensitive information              |
| user_msg  | If the result is an error, this message will be shown to the user        |
| log_msg  | If the result is an error, this message will be logged        |


The entries in the credential object (part of the list in 'credential' above) are:

| Key   | Description                                                                                                       |
|-------|-------------------------------------------------------------------------------------------------------------------|
| name  | The name the user will see e.g. username or password                                                              |
| type  | The type of data, this will determine how it will be displayed. At the moment 'text' and 'textfile' are supported |
| rows  | The number of rows for the textarea in case the type is textfile, else ignored                                        |
| cols  | The number of columns for the textarea when using textfile, else ignored                                          |
| value | The value of the credential part, e.g. the password or the username itself                                         |


#### Performing a Revoke
When the `action` is set to `revoke`, the credential identified by the `state` should be invalidated
or removed, depending on the type of service. In the SSH plugin, e.g., the created ssh-key is
removed from the authorized keys.
When performing a request, the TTS expects the stdout to be a json object. In case of a revoke, it may contain
one of the two elements:
```
{
	"result":"ok"
}
```
or in case of an error
```
{
	"result":"error",
    "user_msg","Sorry, could not revoke the certificate",
    "log_msg","internal error when executing command ...."
}
```
| Key    | Description                                                             |
|--------|-------------------------------------------------------------------------|
| result | either 'ok' or 'error', representing the result of the plugin           |
| user_msg  | If the result is an error, this message will be shown to the user        |
| log_msg  | If the result is an error, this message will be logged        |


Therefore, if only the result key is present in the object, the TTS assumes that
everything went fine, however, if the error key is present it assumes that
something went bad and does not remove the credential state from its database.
