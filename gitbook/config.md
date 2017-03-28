# Configuration Guide
The configuration of WaTTS consists of one file. The file is located at `/etc/watts/watts.conf`.
One other location is supported for development purposes; in this case please place your
configuration file as `~/.config/watts/watts.conf`.


## Configuration (watts.conf)
The WaTTS is deployed with sane defaults, you only need to touch user-specific
changes.

Each setting consists of one simple line of the format, comments are starting with '#'.

```
# this is a comment and ignored
key = value
```


### Testing your configuration
WaTTS provides a simple command to check the configuration file
```
watts chkconfig
```
run it after changing the configuration to ensure your configuration file is correct.
If everything is fine it will print out a line telling you so:
```
config is OK
```

### Datatypes
There are different datatypes used in the configuration, a detailed description can be
seen in the following table.

| Datatype | Description |
| :---: | --- |
| 'word'| the word itself is the value, without the ' |
| host | a valid fully qualified hostname |
| port | an integer within the valid range for TCP ports |
| boolean | either 'true' or 'false' |
| file | an absolute path to a file |
| duration | a timespan given by an integer and a unit, the unit can be ms, s, m, h |
| string | just the string value |
| integer | an integer value, i.e. a number |
| url | a valid url, use https as much as possible |
| comma separated list | values separated by comma |
| any | depends on the usage and can't be specified |



### WaTTS server settings
#### Introduction
This section will describe the general settings of the WaTTS server. This will include options like
ports, hostname and SSL.


Typical values that should be changed during the initial setup are:

- `hostname`, change it to an actual fully qualified hostname
- `port`, which can be removed if the incoming traffic arrives at port *80* for *http* or *443* for *https*
- `listen_port`, it will be set to the internal port WaTTS is listening at

And for production use:

- `ssl`, set to 'true'
- `cachain_file`, set to the path to the file
- `cert_file`, set to the path to the file
- `key_file`, set to the path to the file

#### Settings
| Key | Description | Datatype | Default |
| :---: | --- | :---: | :---: |
| hostname | Hostname of the web server | host | localhost |
| port | Port number where clients seem to connect to; default is port 80 for non SSL, 443 for SSL. In production systems this should be left 'default' | port number or 'default' | 8080 |
| listen_port | Port at which WaTTS actually listens, used to support listening at non-privileged ports; the traffic must then be redirected from the privileged ports to the listen_port usually by the firewall. The value 'port' means using the same value as `port`| port or 'port' | 'port' |
| ssl | Whether SSL should be used | boolean | true |
| cachain_file | Location of the ca chain for the server  | file | none |
| cert_file | Location of the certificate  | file | /etc/watts/watts.crt |
| key_file | Path to the private key file | file | /etc/watts/watts.key |
| session_timeout | The duration for which a session at the web-app is valid during inactivity | duration | 15m |
| session_max_duration | The maximum duration of a session at the web interface, even if active. After this time an additional Login is required | duration | 30m |
| sqlite_file | Path to the sqlite database | file | /etc/watts/watts.db |
| redirection.enable | Whether redirection should be enabled | boolean | false |
| redirection.listen_port | The port to listen on for browsers to redirect | port | 8080 |
| allow_dropping_credentials | Whether credentials of unknown services can be silently dropped | boolean | false |
| enable_docs | Whether the documentation is reachable at /docs/ | boolean | false |



#### Listen_Port, Redirection Explained
The idea is to run WaTTS as a dedicated *non root* user for security reasons.
The drawback of not beeing root is that ports 1-1024 are not available to WaTTS.
To still be able to have WaTTS running at port 80 or 443 several settings are needed.

As an image tells more than a thousand words, soma ascii art:
```
client --[port]---> firewall rules --[listen_port]--> WaTTS
```
In the picture above the client connects to the port `port` and firewall rules
redirect the packages arriving at `port` to the `listen_port` at which WaTTS is actually listen.
The corresponding firewall rule is:
```
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport `port` -j REDIRECT --to-port `listen_port`
```

Redirection is needed when using SSL and http traffic should be forwarded to the https endpoint.
The problem is that http and https work completely different, so a pure redirection using the
firewall does not work, instead a valid http-redirection message needs to be send. Sending
this valid http message is the task of the redirection and needs to be listening at a different port:
```
client --[some port]--> firewall rules --[redirection.listen_port]--> redirection endpoint
                                                                           |
       <-------[ valid http message, redirecting to the http endpoint ]----/
```
The redirection follows the same idea as the port and listen_port above. So WaTTS is listening
at redirection.listen_port for incomming traffic and sending a valid http redirection message back,
which tells the browser to go tho the ssl endpoint: https://`hostname`:`port`.

For the redirection another firewall rule is needed:
```
# redirecting all traffic arriving at the default http port, 80, to the the listen port
# for redirection
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j REDIRECT --to-port `redirection.listen_port`
```

#### Example
The following example is the basic SSL setup.
```
hostname = my-watts.example.com
listen_port = 8443
port = 443
ssl = true
# using default values for cachain_file, cert_file and key_file
session_timeout = 10m
redirection.enable = true
redirection.listen_port = 8000
```

and the firewall rules
```
# for the ssl traffic forwarding from 443 to the listen port of WaTTS
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 443 -j REDIRECT --to-port 8443

# for the non ssl traffic trying port 80 forwarding it to the redirection.listen_port of WaTTS
iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j REDIRECT --to-port 8000
```


### OpenId Connect Provider
#### Introduction
To provide a login mechanism for the user, at least one OpenId Connect Provider
is needed.

WaTTS needs to be registered as a client with OpenId Connect Provider. For this,
you need to perform the registration process at the Provider of your choice. The
registration process depends heavily on the Provider and is out of the scope of this
documentation, if you are unsure you can ask the provider.

During the registration, some information needs to be provided.
The redirect *uri* is created from three settings:

- `ssl`: http:// (false, default) or https:// (true)
- `hostname`: localhost (default)
- `port`: 8080 (default)
- fix path: /oidc

For the default settings this results in the redirect uri:
http://localhost:8080/oidc.

The redirect uri for the settings 'SSL = true', 'Port = 443', 'HostName=tts.example.com'
would be https://tts.example.com/oidc (the port is not added as it is the default
port for https, it would be the same for port 80 on SSL = false).

If you are unsure, just start the WaTTS and check the logs. During the start of WaTTS, it prints
some messages starting with `Init:`, one of them is `Init: using local endpoint ....`
telling you which uri to use.

WaTTS uses the 'code-auth-flow' and is a 'web-application'.

#### General Settings
WaTTS verifies the complete SSL chain if https is used and is very strict to ensure the
integrity for the user using WaTTS.

To be able to verify a remote certificate WaTTS needs to know where the supported certification
authority certificates are stored. The second information WaTTS needs is the allowed depth of
CA and intermediate CAs to have before reaching the server certificate.


| Key | Description | Datatype | Default |
| :---: | --- | :---: | :---: |
| oidc.cacertfile | The file containing all trusted CAs | file | none |
| oidc.cert_depth | The number of intermediate CAs allowd | integer | 1 |
| oidc.cache_duration | The time, in seconds, http request to userinfo/tokeninfo are allowed to get cacheed. The purpose is to keep high load from the OpenId Connect Provider. Yet this also means that there is a 90 second window in which a user might still be allowed to perform actions although he has been blocked at the IdP | integer, 'none' | 90 |
| oidc.cache_clean | The amount of time (in seconds) to wait before cleaning up the cache | integer | 300 |
| oidc.request_timeout | The number of seconds an http request is allowed to take | integer | 300 |
| oidc.use_cookie | If cookies should be used to identify the user during login | boolean | true |
| oidc.check_user_agent| Wether the user agent identifier should be verified  | boolean | true |
| oidc.check_peer_ip | Should the remote IP be checked when logging in the user | boolean | true |

#### Example
```
# this should be the setting on debian based systems if you want to trust the default bundle
oidc.cacertfile = /etc/ssl/certs/ca-certificates.crt
# this should be the setting on centos systems if you want to trust the default bundle
oidc.cacertfile = /etc/ssl/certs/ca-bundle.crt

# on both systems you can cange the allowed depth by
oidc.cert_depth = 5
```


#### Provider Settings

| Key | Description | Datatype |
| :---: | --- | :---: |
| description | A description of the Provider, shown at the login Screen | string |
| client_id | The client id received at the registration | string |
| client_secret | The client secret received at the registration | string |
| config_endpoint | The configuration endpoint of the provider, ensure you are using ssl | url |
| request_scopes | the scopes to request | comma separated list |

Each setting is prefixed with 'openid.`id`.' where `id` must be replaced by the id
you want to give to the provider.

#### Example
An example for the IAM OpenId Connect Provider, setting its id to `iam`:
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
The configuration of a service consists of one group of *service* settings.

To create credentials, WaTTS connects to the service, either locally or
remotely using *ssh*. After the connection is established, a command is
executed and the subsequent result is parsed and interpreted.

The executed commands are also called *plugins*; for further information on how
the plugins work and how to implement them, see the documentation for developers.

The plugins are not part of WaTTS, so they can be changed independant of WaTTS and
also maintained by the community.

Known plugins(use them at your own risk):
 - [Info plugin](https://github.com/indigo-dc/tts_plugin_info)
 - [OpenNebula](https://github.com/indigo-dc/tts_plugin_opennebula)
 - [Simple CA](https://github.com/indigo-dc/tts_plugin_ca)

Feel free to add an issue to get your plugin listed above.


#### Settings
Each setting is prefixed with 'service.`id`.' where `id` must be replaced by the id
you want to give to the service.

| Key | Description | Datatype | Mandatory |
| :---: | --- | :---: | :---: |
| description | A description of the service for the user | string | yes |
| cmd | The command to execute after connecting, needs to be executable/readable by the user WaTTS is running as | string | yes |
| credential_limit | The maximum number of retrievable credentials | integer or 'infinite' | no (infinite) |
| parallel_runner | The number of parallel runs of the plugin for the service | integer or 'infinite' | no, (1) |
| allow_same_state | Whether the plugin is allowed to return the same state more than once | boolean | no, (false) |
| plugin_timeout | The time after which WaTTS won't wait for the result of the plugin execution anymore | duration or 'infinity' | no (infinity) |
| pass_access_token | Whether the  access token should be passed to the plugin. Enabling this is adding a security risk! | boolean | no (false) |
| connection.type | Either local or ssh | 'local' or 'ssh' | yes |
| connection.user | The user name to use when connecting e.g. with ssh | string | no |
| connection.password | The password to use when connecting e.g. with ssh | string | no |
| connection.host | The hostname to connect to e.g. with ssh | host | no |
| connection.port | The port number to connect to e.g. with ssh | port | no |
| connection.ssh_dir | The ssh_dir to use with ssh | port | no |
| connection.ssh_key_pass | The password of the private key to use with ssh | string | no |
| plugin.`key` | A setting to send to the plugin, the name of the parameter will be `key`. Which values and which keys are supported depend on the plugin. If no parameter are set warning about using default values are issued in the logs | any | no |
| authz.allow.`p`.`k`.`o` | See the Authorization section | other | no ([])|
| authz.forbid.`p`.`k`.`o` | See the Authorization section | other | no ([]) |
| authz.hide | Hide the service if the user is not allowed | boolean | no (false) |
| authz.tooltip | Message that is shown when hovering the row of the service that is not allowed, used to give users a hint on how they might get access to the service | boolean | no (false) |



#### Authorization
Authorization follows a few simple steps:
 1. everyone can be forbidden
 2. if an `allow` rule that matches the user is true, she is allowed
 3. if a `forbid` rule that matches the user is true, he is forbidden

So a for a user to access a service she:
 - MUST match at least one `allow` rule that evaluates to true
 - MUST NOT match any `forbid` rule that evaluates to true

Each rule is exactly one line long. A rule always consists of five parts:
` authz.allow.p.k.o = v`, where the values are:

 - `p`: the Id of the provider
 - `k`: the key within the OpenId Connect Id-Token or user information
 - `info`: the information in the Id Token or user information with the key `k`
 - `o`: the operation to perform
 - `v`: the value

The provider Id is the same as was given during the configuration, in the provider example above
the *id* was `iam`, so using that for `p` allows making decisions on users coming from `iam`.
There is a special provider *id* value, `any`, which matches any provider.


The key `k` has to match a value of the *id* token or the user info. The value of the
*Id Token* offers *Userinfo*, i.e. having the key `k` is the `info`.
If the key is not present:
 - `allow` rule evaluates to *false*
 - `forbid` rule evaluates to *true*

The operation `o` can be one of the following list:
 - `contains`: the `info` can either be a list or a string
   - a *list*: the value `v` must be a member of the list `info`
   - a *string*: the value `v` must be part of the string `info`
 - `is_member_of`: the value `v` must be a comma separated list (with no spaces!) and `info` needs to be a member of that list
 - `equals`: `v` and `info` need to be equal
 - `regexp`: `v` is a regular expression and `info` needs to satisfy the expression
 - `any`: evaluates to `v`, so to make this pass set `v` to 'true'

#### Examples
```
# 'p' is any, so matching all providers
# 'k' is sub, the subject of the id token, this is always present
# 'info' can be ignored due to
  # 'o' being 'any'
  # 'v' is true
# The following line is an example allowing anyone from any provider:
service.info.authz.allow.any.sub.any = true


# 'p' is iam, so matching only the provider with the id iam
# 'k' is sub, the subject of the id token, which for sure is always present
# 'info' can be ignored due to
  # 'o' being 'any'
  # 'v' is true
# The next line shows an example allowing anyone from the iam provider:
service.info.authz.allow.iam.sub.any = true

# The examples below will concentrate on the operations.

# 'k' is a group, i.e. the groups the user belongs to
# 'info' should be a list
# 'o' has value 'contains'
# 'v' is 'Developer'
# The following line allows anyone whose group list contains 'Developer' and is
# from the iam provider
service.info.authz.allow.iam.group.contains = Developer

# 'k' is sub
# 'info' is the subject within iam
# 'o' being 'is_member_of'
# 'v' is a comma separated list of subjects
# The following line allows sub1, sub2 and sub3 from the provider iam
service.info.authz.allow.iam.sub.is_member_of = sub1,sub3,sub2
```

#### Complete Example
```
service.info.description = Simple Info Service
service.info.credential_limit = 1
service.info.connection.type = local
service.info.cmd = /home/watts/.config/watts/info.py
service.info.parallel_runner = infinite
service.info.authz.allow.any.sub.any = true
# the following will be sent to the plugin
service.plugin.path = /tmp/data
```


### Complete Config Example
This example uses the info plugin, which is a very simple plugin and can be downloaded [here](https://github.com/indigo-dc/tts_plugin_info).
```
hostname = watts.example.com
port = 443
listen_port = 8443
redirection.enable = true
redirection.listen_port = 8080

ssl = true
cachain_file = /home/watts/cert/ca-bundle.crt
cert_file = /home/watts/cert/server.crt
key_file = /home/watts/cert/server.key

oidc.cacertfile = /etc/ss/certs/ca-certificates.crt
oidc.cert_depth = 3

sqlite_file = /home/watts/watts.db

openid.iam.description = INDIGO Datacloud Identity and Access Management (IAM)
openid.iam.client_id = e70e7190-d64cad3771b9
openid.iam.client_secret = AN5pY379_Z_1jgiiAyKBEHe8zKP2KuiWxV34zWzw
openid.iam.config_endpoint = https://iam.indigo-datacloud.eu/.well-known/openid-configuration
openid.iam.request_scopes = openid, profile

service.info.description = Simple Info Service
service.info.credential_limit = 1
service.info.connection.type = local
service.info.cmd = /home/watts/info.py
service.info.parallel_runner = infinite
service.info.authz.allow.any.sub.any = true
```



### Configuring SSH for WaTTS
WaTTS does not yet support hashed hosts in the `known_hosts` file. As the
connection to the remote host is done without user interaction, the host MUST be
listed in the `known_hosts` file.

To add a host to the list of known hosts in a way readable for WaTTS, the
`ssh_config` (usually located in `/etc/ssh/ssh_config`) must have the setting
`HashKnownHosts no`. After checking and eventually updating the configuration,
one can login as a WaTTS user.

As a WaTTS user, one connects to remote hosts using the credential specified in the
service configuration, with potentially using the verbose flag (-v). During the
connection there are two possibilities:

1. The client asks whether the host should be added. In the case it is a host that
   should be accessible by WaTTS, the user should answer with yes, and then
   can go on with the next host.
2. The client silently connects without asking. If this is the case, the host is
   already in the `~/.ssh/known_hosts` file. In the verbose connection, the
   output will tell which line in the file belongs to the remote host. In this
   case you can edit the `known_hosts` file and delete the line at the number
   printed before. Save the file and start the connection again, and the
   situation described above will happen.

After adding all the hosts, the `ssh_config` should be modified. Open the
`ssh_config` and change the hash hosts setting to `HashKnownHosts yes`.
