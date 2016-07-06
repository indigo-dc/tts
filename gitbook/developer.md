# Developer Guide
## Plugins
To enable a certain type of service for the TTS it needs a corresponding plugin. Sometimes there are even multiple plugins for the same type of service e.g. due to different local policies.

### Writing Plugins
The communication between the TTS and the plugin is done by passing one argument to the script and parsing the std-out of the script.

#### Plugin Input
The parameter coming from the TTS is an base64 encoded json object.

The decoded json object has the following format:
```
{
"action":"request",
"cred_state":"undefined",
"params":[],
"user_info":{
	"oidc":{
		"email":"alice.wonderland@indigo-dc.eu",
		"iss":"https://iam.indigo-dc.eu",
		"locale":"wl",
		"name":"Alice Wonderland",
		"sub":"123456789",
		"email_verified":true,
		"family_name":"Wonderland",
		"gender":"female",
		"given_name":"Alice",
		},
	"site":{
		"gidNumber":1000,
		"homeDirectory":"/home/alice",
		"uid":"alice",
		"uidNumber":1000,
		"userIds":{"https://iam.indigo-dc.eu":"123456789"}
		},
	}
}
```
The items of the object are:
| Key        | Description                                                                                         |
|------------+-----------------------------------------------------------------------------------------------------|
| action     | The action to perform, this can be either 'request' or 'revoke'                                     |
| cred_state | This is only passed when revoking, it is the credential state returned when creating the credential |
| user_info  | The user info contains two distince informations about the user: oidc and site (see below)          |
| oidc       | The information about the user coming from the OpenId Connect provider e.g. IAM                     |
| site       | The information coming from the IDH script about the user                                           |

#### Performing a Request
When the `action` is set to `request` it means that the user or a service on the
users behalf wats to create a credential.
The plugin should perform any action needed to create the credential for the user and print
a json object on stdout.

The expected format of the json object is:
```
{
	"error":"some bad thing happend",
	"credential": [{"name":"some name", "type":"text", "value":"secret"}],
	"state":"state description"
}
```

| Key        | Description                                                                                    |
|------------+------------------------------------------------------------------------------------------------|
| credential | a list of object each representing one part of the credentials. This will be shown to the user |
| state      | a state to keep track of this credential it MUST NOT contain sensive informations              |
| error      | if this is present the TTS expects that something bad happens and ignores the other two  |

The entries in an credential object (part of the list in 'credential' above) are:
| Key   | Description                                                                                                       |
|-------+-------------------------------------------------------------------------------------------------------------------|
| name  | the name the user will see e.g. username or password                                                              |
| type  | the type of data, this will determine how it will be displayed. At the moment 'text' and 'textfile' are supported |
| rows  | the number of rows for the textarea in case of type textfile, else ignored                                        |
| cols  | the number of columns for the textarea when using textfile, else ignored                                          |
| value | the value of the credential part e.g. the password or the username itself                                         |


#### Performing a Revoke
When the `action` is set to `revoke` the credential identified by the `state` should be invalidated 
or removed, depending on the type of service. In the SSH plugin the created ssh-key gets e.g. 
removed from the authorized keys.
As with requesting the TTS expects the std out to be a json object. In case of revoke it may contain
one of the two elements:
```
{
	"error":"some bad thing happend",
	"result":"description"
}
```
| Key    | Description                                                             |
|--------+-------------------------------------------------------------------------|
| result | a description of the result, at the moment it is ignored                |
| error  | if the error is present the TTS expects an error and ignores the result |

So if only the result key is present in the object the TTS assumes that everything went fine,
if the error key is present it assumes that something went bad and does not remove the credential 
state from its database.
