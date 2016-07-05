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
| Port | The port number to be used for connections default means: 80 for non
SSL, 443 for SSL | default |
| ListenPort | the port the server is listening at, used if the TTS listens at a
non priviliged port and the traffic is then redirected from the priviliged ones
e.g. 80 or 443 | default |
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
* MAX_UID: the latest uid to use for TTS users
* CREATE_LOCAL_ACCOUTNS: wether accounts should be created at the TTS server


### OpenId Connect Provider 
To provide a login mechanism for the user at least one OpenId Connect Provider
is needed. 

The files reside in the `oidc` subdirectory of the tts configuration and one
file per provider is used. The filename has to end on `.conf`.

The possible settings are:

| Key | Description | Default |
| :---: | --- | :---: |
| Id | The Id to refer to this Provider | randomly generated |
| Description | A description of the Provider, shown at the login Screen | none |
| ClientId | The client id received at registration | none |
| Secret | The client secret received at registration | none |
| ConfigEndpoint | The configuration endpoint of the provider | none |

An example for the IAM OpenId Connect Provider:
```
Id = IAM
Description = INDIGO Datacloud Identity and Access Management (IAM)
ClientId = <insert the client id> 
Secret =  <put your client secret here> 
ConfigEndpoint = https://iam-test.indigo-datacloud.eu/.well-known/openid-configuration 
```

### Services 

