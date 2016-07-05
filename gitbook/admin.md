# Deployment And Administration Guide 
## Installation
### From Package 
For Ubuntu 14.04 and CentOS 7 a package is provided by the INDIGO DataCloud
team.
To be able to install the packages using the package manager of your system the 
repository needs to be added. This is done by adding the INDIGO DataCloud
package repository to your system.

For informations on how to add the repository to your system refer to the
documentation at @TODO

#### Ubuntu 14.04 
After adding the repository one needs to update the package list and then install
the Token Translation Service.
```
apt update
apt install tts
```

#### CentOS 7
After adding the repository one needs to update the package list and then install
the Token Translation Service.
```
yum update
yum install tts
```

### From Source
To be able to install the Token Translation Service from source you need Erlang
OTP 18.1 or newer installed.

Prebuilt package for your operation system can be found at [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).
Just download and install it.

Then follow these few steps:
- clone the git repository
- build the package yourself
- install the package
```
git clone https://github.com/indigo-dc/tts
cd tts
make clean
make package
```
the package can be found in `./package/packages`.

## Configuration
The configuration files of the Token Translation Service are usually located at
`/etc/tts`. Other places are also supported for e.g. development purposes, in
this case please put your configuration files in `~/.config/tts`. If the later
location is found it will override the global configuration.

### Basic Configuration (main.conf)
The main configuration for the TTS is `main.conf`.

| Key | Description | Default |
| :---: | --- | :---: |
| HostName | The hostname to be used for the web server  |localhost |
| Port | The port number to be used for connections default means: 80 for non SSL, 443 for SSL | default |
| ListenPort | the port the server is listening at, used if the TTS listens at a non priviliged port and the traffic is then redirected from the priviliged ones e.g. 80 or 443 | default |
| SSL | Wether SSL should be used | true |
| CaCertFile | the location of the CA file, if not absolute it is relative to the config path  | cert/ca.cert |
| CertFile | location of the certificate (see above) | cert/tts.cert |
| KeyFile | the path to the private key file (see CaFile) | cert/tts.key |
| SqliteFile | path to the sqlite database | ./tts.db |
| SessionTimeout | Timeout of a Session at the web interface (in seconds) | 600 |
| CacheTimeout | Timeout of cached user information (seconds) |  900 |
| CacheCheckInterval | Cache validation and cleanup interval (seconds) |  300 |
| CacheMaxEntries | Number of entries to keep in the cache at max | 50000|
| ServiceConfigPath | the directory where the service configs are stored, relative to the main config| ./services |
| OidcConfigPath | directory of the OpenId Connect Provider configs | ./oidc |
| IDHScript | The Identiy Harmonization Script (IDH) to use | ./idh.py |
| IDHMaxWorker | Max amoutn of workers looking up user data in parallel | 5 |


### Identity Harmonization (IDH)
The purpose of the IDH script is to lookup or create site specific accounts for
the OpenId Connect user. 

Provided with the Token Translation Service is a baisc IDH script, which uses a
sqlite database to keep track of virtually created users.

The script is located at `/usr/share/tts/idh/baisic-idh.py` and contains a few
settings. These settings can be changed in the file by modifing it. The most 
important settings are:
* MIN_UID: the minimal uid to use for TTS users
* MAX_UID: the latest uid to use for TTS users, set to 0 for unlimited
* CREATE_LOCAL_ACCOUTNS: wether accounts should be created at the TTS server


### OpenId Connect Provider 
To provide a login mechanism for the user at least one OpenId Connect Provider
is needed. 

The files reside in the `oidc` subdirectory of the TTS configuration and one
file per provider is used. The filename has to end on `.conf`.

The possible settings are:

| Key | Description | Mandatory |
| :---: | --- | :---: |
| Id | The Id to refer to this Provider | no (randomly generated) |
| Description | A description of the Provider, shown at the login Screen | yes |
| ClientId | The client id received at registration | yes |
| Secret | The client secret received at registration | yes |
| ConfigEndpoint | The configuration endpoint of the provider | yes |

An example for the IAM OpenId Connect Provider:
```
Id = IAM
Description = INDIGO Datacloud Identity and Access Management (IAM)
ClientId = <insert the client id> 
Secret =  <put your client secret here> 
ConfigEndpoint = https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration 
```

### Services 
A service is a single entity for which a user can request credentials.
The configuration of a service consist of one `.conf` file per service, which
are located in the `services` subdirectory of the configuration.

To create credentials the TTS connects to that service, either locally or
remotely by using ssh. After the connection is established a given command gets
executed and its result parsed and interpreted.

The commands executed are also called plugins, for further informations on how
plugins work and how to implement them see the documentation for developers.

List of parameters:

| Key | Description | Mandatory |
| :---: | --- | :---: |
| Id | The id to internally use when refering to this service | yes |
| Type | A type displayed to the user | yes |
| Host | A host displayed to the user | yes |
| Port | A port shown to the user | yes |
| Description | A description of the service for the user | yes |
| CredentialLimit | The maximal number of credentials retrievable | yes |
| Cmd | The command to execute after connecting | yes |
| AllowSameState | Allow the plugin to return the same state, usually not needed | no |
| ConnectionType | Either local or ssh | yes |
| ConnectionHost | the host to connect to | no |
| ConnectionPort | the port to connect to | no |
| ConnectionUser | the user to use when connecting | no |
| ConnectionPassword | the password to use when connecting | no |
| ConnectionSshDir | the directory for known_hosts and keys | no |
| ConnectionSshKeyPass | password for the private key | no |
| ConnectionSshKeyAutoAcceptHosts | *WARNING* this introduces a security hole, autmatically accept all ssh hosts | no |

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

A more advanced example with ssh:
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
This assumes a `.ssh` folder to be present at `~/.ssh/` with ssh.examplel.com 
listed in `known_hosts` and at least one key file, encrypted using the passphrase 
given with `ConnectionSshKeyPass`. The home directory in this case is the one of
the user running the TTS.
