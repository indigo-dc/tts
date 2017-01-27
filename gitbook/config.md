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

### Datatypes
There are different datatypes used in the configuration, a detailed description can be
seen in the following table.
| Datatype | Description |
| :---: | --- |


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


#### Example
```
hostname = my-watts.example.com
listen_port = 8443
port = default
ssl = true
# using default values for cachain_file, cert_file and key_file
session_timeout = 10m
redirection.enable = true
redirection.listen_port = 8000
```

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

#### Example
An example for the IAM OpenId Connect Provider, setting its id to 'iam':
```
openid.iam.description = INDIGO Datacloud Identity and Access Management (IAM)
openid.iam.client_id = <insert the client id>
opemid.iam.client_secret =  <insert the client secret>
openid.iam.config_endpoint = https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration
openid.iam.request_scopes = openid, profile
```

### Services
#### Introduction
A service is a single entity for which a user can request credentials.
The configuration of a service consist of one group of 'service' settings.
are located in the `services` subfolder of the configuration.

To create credentials, WaTTS connects to the service, either locally or
remotely using ssh. After the connection is established, a command is
executed and the subsequent result is parsed and interpreted.

The executed commands are also called plugins; for further information on how
the plugins work and how to implement them, see the documentation for developers.

#### Settings
| Key | Description | Datatype | Mandatory |
| :---: | --- | :---: | :---: |
| description | A description of the service for the user | string | yes |
| cmd | The command to execute after connecting | string | yes |
| credential_limit | The maximum number of retrievable credentials | integer or 'infinite' | yes |
| connection_type | Either local or ssh | 'local' or 'ssh' | yes |
| parallel_runner | the number of parallel runs of the plugin for the service | integer or 'infinite' | no, (1) |
| allow_same_state | wether the plugin is allowed to return the same state more than once | boolean | no, (false) |
| plugin_timeout | the time after wich WaTTS won't wait for the result of the plugin anymore | duration or 'infinity' | no (infinity) |
| pass_access_token | wether the  access token should be passed to the plugin | boolean | no (false) |
| connection.user | the user to use when connecting e.g. with ssh | string | no |
| connection.password | the password to use when connecting e.g. with ssh | string | no |
| connection.host | the host to connect to e.g. with ssh | host | no |
| connection.port | the port to connect to e.g. with ssh | port | no |
| connection.ssh_dir | the ssh_dir to use with ssh | port | no |
| connection.ssh_key_pass | the password of the private key to use with ssh | string | no |
| plugin.`key` | a setting to send to the plugin, the name of the parameter will be `key` | any | no |
| authz.allow.`p`.`k`.`o` | see the Authorization section | other | no ([])|
| authz.forbid.`p`.`k`.`o` | see the Authorization section | other | no ([]) |
| authz.hide | hide the service if the user is not allowed | boolean | no (false) |
| authz.tooltip | the message shown when hovering the row of the service and not allowed, used to give users a hint on how they might get access to the service | boolean | no (false) |


Each setting is prefixed with 'service.`id`.' where `id` must be replaced by the id
you want to give to the service.
#### Example
```
service.info.description = Simple Info Service
service.info.credential_limit = 1
service.info.connection.type = local
service.info.cmd = /home/watts/.config/watts/info.py
service.info.parallel_runner = infinite
# see the Authorization section regarding the next line
service.info.authz.allow.any.sub.any = true
```

#### Authorization
Authorization follows a few simple steps
 1. everyone is forbidden
 2. if a `allow` rule which matches the user is true she is allowed
 3. if a `forbid` rule which matches the user is true he is forbidden

So a for a user to access a service she
 - MUST match at least one `allow` rule and evaluate to true
 - MUST NOT match any `forbid` rule  that evaluates to true

Each rule is exactly one line. A rule consists always aut of five parts:
` authz.allow.p.k.o = v `
 - `p`: the Id of the provider
 - `k`: the key within the OpenId Connect Id-Token or user information
 - `info`: the information in the Id Token or user information with the key `k`
 - `o`: the operation to perform
 - `v`: the value

The provider Id is the same as given during configuration, in the provider example above
the id was 'iam' so using that for `p` would allow to decide on users coming from iam.
There is a special provider id, which is `any` and it matches any provider.


The key `k` has to match a value of the id token or the user info. The value of the
Id Token ofer Userinfo having the key `k` is the `info`.
if the key is not present:
 - an allow rule evaluates to false
 - a forbid rule evaluates to true

the operation `o` can be one of the following list:
 - contains: the `info` can either be a list or a string
   - a list: the value `v` must be a member of the list `info`
   - a string: the value `v` must be part of the string `info`
 - is_member_of: the value `v` must be a comma separated list (with no spaces!) and `info` needs to be a member of that list
 - equals: `v` and `info` need to be equal
 - regexp: `v` is a regular expression and `info` needs to satisfy the expression
 - any: evaluates to `v`, so to make this pass set `v` to 'true'

#### Examples
```
# 'p' is any, so matching all provider
# 'k' is sub, the subject of the id token, which for sure is always present
# 'info' can be ignored due to
# 'o' being 'any'
# 'v' is true
# so the following line allows anyone from any provider
service.info.authz.allow.any.sub.any = true


# 'p' is iam, so matching only the provider with the id iam
# 'k' is sub, the subject of the id token, which for sure is always present
# 'info' can be ignored due to
# 'o' being 'any'
# 'v' is true
# so the following line allows anyone from the iam provider
service.info.authz.allow.iam.sub.any = true

# the following examples will concentrate on the operations

# 'k' is group, so the groups the user belongs to
# 'info' should be a list
# 'o' being 'contains'
# 'v' is 'Developer'
# so the following line allows anyone from the iam provider whos group
# list contains 'Developer'
service.info.authz.allow.iam.group.contains = Developer

# 'k' is sub
# 'info' is the subject within iam
# 'o' being 'is_member_of'
# 'v' is a comma separated list of subjects
# so the following line allows sub1, sub2 and sub3 from the provider
# iam
service.info.authz.allow.iam.sub.is_member_of = sub1,sub3,sub2
```

### Configuring SSH for WaTTS
WaTTS does not yet support hashed hosts in the `known_hosts` file. As the
connection to the remote host is done without user interaction the host MUST be
listed in the `known_hosts` file.

To add a host to the list of known hosts in a way readable for WaTTS the
`ssh_config` (usually at `/etc/ssh/ssh_config`) must have the setting
`HashKnownHosts no`.  After checking and eventually updating the configuration
login as the WaTTS user.

As WaTTS user connects to the remote hosts using the credential specified in the
service configuration, and potentially using the verbose flag (-v). During the
connection there are two possibilities:
1. The client asks whether the host should be added. In case it is a host that
   should be accessible by WaTTS, the user should answer with yes, and then
   can go on with the next host.
2. The client silently connects without asking. If this is the case, the host is
   already in the `~/.ssh/known_hosts` file. In the verbose connection, the
   output will tell which line in the file belongs to the remote host. Open the
   `known_hosts` file and delete the line at the number printed before. Save the
   file and start the connection step again.

After adding all the hosts, the `ssh_config` should be modified. Open the
`ssh_config` and change the hash hosts setting to `HashKnownHosts yes`.
