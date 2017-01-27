# Configuration Guide
The configuration of WaTTS consists of one file. The file is located at `/etc/watts/watts.conf`.
One other locations is supported for development purposes; in this case please place your
configuration file at `~/.config/watts/watts.conf`.


## Configuration (watts.conf)
The WaTTS comes shipped with sane defaults, you only need to touch settings that you
want to change.

Each setting consists of one simple line of the format, comments are starting with '#'
```
# this is a comment and ignored
key = value
```

### WaTTS server settings
#### Introduction
This section will describe the general settings of the WaTTS server. This will include options like
ports, hostname and SSL.


Typical values that should be changed during the initial setup are:
- `hostname`, changing to the actual fully qualified hostname
- `port`, can be removed if the incomming traffic will arrive at port 80 for http or 443 for https
- `listen_port`, will be set to the internal port WaTTS is listening at

And for production use:
- `ssl`, set to 'true'
- `cachain_file`, set to the path to the file
- `cert_file`, set to the path to the file
- `key_file`, set to the path to the file

#### Settings
| Key | Description | Datatype | Default |
| :---: | --- | :---: | :---: |
| hostname | Hostname of the web server | hostname | localhost |
| port | Port number where clients seem to connect to; deault means port 80 for non SSL, 443 for SSL. On production systems this should be 'default' | port or 'default' | 8080 |
| listen_port | Port at which WaTTS actually listens, used to support listening at non-privileged ports; the traffic must then be redirected from the privilidged ports to the listen_port usually by the firewall. The value 'port' means using the same value as `port`| port or 'port' | 'port' |
| ssl | Whether SSL should be used | boolean | false |
| cachain_file | Location of the ca chain for the server  | file | none |
| cert_file | Location of the certificate  | file | /etc/watts/watts.crt |
| key_file | Path to the private key file | file | /etc/watts/watts.key |
| session_timeout | the duration a session at the web-app is valid | duration | 15m |
| sqlite_file | Path to the sqlite database | file | /etc/watts/watts.db |
| redirection.enable | Wehter redirection should be enables | boolean | false |
| redirection.listen_port | the port to listen on for browsers to redirect | port | 8080 |
| allow_dropping_credentials | wether credentials of unknown services can be silently dropped | boolean | false |




### OpenId Connect Provider
#### Introduction
To provide a login mechanism for the user, at least one OpenId Connect Provider
is needed.

WaTTS needs to be registered as a client at an OpenId Connect Provider. For this
you need to perform the registration process at the Provider of your choice. The
registration process heavily depends on the Provider and is out of the scope of this
documentation, if you are unsure you can ask the provider.

During the registration some informations need to be provided.
The redirect uri is created from three settings:
- `ssl`: http:// (false, default) or https:// (true)
- `hostname`: localhost (default)
- `port`: 8080 (default)
- fix path: /oidc

For the default settings this results in the redirect uri:
http://localhost:8080/oidc.

The redirect uri for the settings 'SSL = true', 'Port = 443', 'HostName=tts.example.com'
would be https://tts.example.com/oidc (the port is not added as it is the default
port for https, it would be the same for port 80 on SSL = false).

If you are unsure just start WaTTS and check the logs. During the start of WaTTS it prints some
some messages starting with 'Init:', one of them is 'Init: using local endpoint ....' which is
telling you the uri to use.

WaTTS uses the 'code-auth-flow' and is a 'web-application'.

#### Settings

| Key | Description | Datatype |
| :---: | --- | :---: |
| description | A description of the Provider, shown at the login Screen | string |
| client_id | The client id received at the registration | string |
| client_secret | The client secret received at the registration | string |
| config_endpoint | The configuration endpoint of the provider | url |
| request_scopes | the scopes to request | comma separated list |

Each setting is prefixed with 'openid.`id`.' where `id` must be replaced by the id
you want to give to the provider.

An example for the IAM OpenId Connect Provider, setting its id to 'iam':
```
openid.iam.description = INDIGO Datacloud Identity and Access Management (IAM)
openid.iam.client_id = <insert the client id>
opemid.iam.client_secret =  <insert the client secret>
openid.iam.config_endpoint = https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration
openid.iam.request_scopes = openid, profile
```

### Services
A service is a single entity for which a user can request credentials.
The configuration of a service consist of one `.conf` file per service, which
are located in the `services` subfolder of the configuration.

The TTS comes with some sample configurations for included plugins.
you can easily test them by renaming them from .sample to .conf.
Three services run out of the box after renaming the file:
- info
- ssh
- x509

To create credentials, the TTS connects to the service, either locally or
remotely using ssh. After the connection is established, a command is
executed and the subsequent result is parsed and interpreted.

The executed commands are also called plugins; for further information on how
the plugins work and how to implement them, see the documentation for developers.

List of parameters:

| Key | Description | Mandatory |
| :---: | --- | :---: |
| Id | The id used internally when referring to this service | yes |
| Type | A type displayed to the user | yes |
| Host | A host displayed to the user | yes |
| Port | A port shown to the user | yes |
| Description | A description of the service for the user | yes |
| CredentialLimit | The maximum number of retrievable credentials | yes |
| Cmd | The command to execute after connecting | yes |
| AllowSameState | Allow the plugin to return the same state, usually not needed | no |
| ConnectionType | Either local or ssh | yes |
| ConnectionHost | Which host to connect to | no |
| ConnectionPort | Which port to connect to | no |
| ConnectionUser | Which user to use when connecting | no |
| ConnectionPassword | Password used when connecting | no |
| ConnectionSshDir | Folder for known_hosts and keys | no |
| ConnectionSshKeyPass | Password for the private key | no |
| ConnectionSshKeyAutoAcceptHosts | *WARNING* This introduces a security issue, automatically accept all ssh hosts | no |

A very basic local example:
```
Id = ssh_local
Type = ssh
Host = localhost
Port = 22
Description = local ssh access to your own machine

Cmd = /usr/share/tts/scripts/ssh.py

ConnectionType = local
CredentialLimit = 3
```

A more advanced example for ssh:
```
Id = ssh_remote
Type = ssh
Host = ssh.example.com
Port = 22
Description = ssh access to example.com

Cmd = /usr/share/tts/scripts/ssh.py

ConnectionType = ssh
ConnectionUser = root
ConnectionHost =  ssh.example.com
ConnectionPort = 22
ConnectionSshKeyPass = secret

CredentialLimit = 3
```
This assumes a `.ssh` folder is present at `~/.ssh/` with ssh.example.com
listed in the `known_hosts` and at least one key file, encrypted using the passphrase
given with the `ConnectionSshKeyPass`. The home folder in this case is the one of
the user running the TTS.

#### Configuring SSH for the TTS
The TTS does not yet support hashed hosts in the `known_hosts` file. As the
connection to the remote host is done without user interaction the host MUST be
listed in the `known_hosts` file.

To add a host to the list of known hosts in a way readable for the TTS the
`ssh_config` (usually at `/etc/ssh/ssh_config`) must have the setting
`HashKnownHosts no`.  After checking and eventually updating the configuration
login as the TTS user.

As TTS user connects to the remote hosts using the credential specified in the
service configuration, and potentially using the verbose flag (-v). During the
connection there are two possibilities:
1. The client asks whether the host should be added. In case it is a host that
   should be accessible by the TTS, the user should answer with yes, and then
   can go on with the next host.
2. The client silently connects without asking. If this is the case, the host is
   already in the `~/.ssh/known_hosts` file. In the verbose connection, the
   output will tell which line in the file belongs to the remote host. Open the
   `known_hosts` file and delete the line at the number printed before. Save the
   file and start the connection step again.

After adding all the hosts, the `ssh_config` should be modified. Open the
`ssh_config` and change the hash hosts setting to `HashKnownHosts yes`.
