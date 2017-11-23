# WaTTS server settings
## Introduction
This section will describe the general settings of the WaTTS server. This will include options like
ports, hostname and SSL.


Typical values that should be changed during the initial setup are:

- `hostname`, change it to an actual fully qualified hostname
- `port`, which can be removed if the incoming traffic arrives at port *80* for *http* or *443* for *https*
- `listen_port`, it will be set to the internal port WaTTS is listening at

And for production use:

- `ssl`, set to 'true'
- `cachain_file`, set to the path to the file
- `dh_file`, set to the path to the file
- `cert_file`, set to the path to the file
- `key_file`, set to the path to the file

## Settings
| Key | Description | Datatype | Default |
| :---: | --- | :---: | :---: |
| nodename | The name of the Erlang distrubuted node. Do not change unless really needed. | string | watts@127.0.0.1 |
| distributed_cookie | The cookie of the Erlang distrubution. Do not change unless really needed. | string | watts |
| hostname | Hostname of the web server | host | localhost |
| port | Port number where clients seem to connect to; default is port 80 for non SSL, 443 for SSL. In production systems this should be left 'default' | port number or 'default' | 8080 |
| listen_port | Port at which WaTTS actually listens, used to support listening at non-privileged ports; the traffic must then be redirected from the privileged ports to the listen_port usually by the firewall. The value 'port' means using the same value as `port`| port or 'port' | 'port' |
| web_acceptors | The number of parallel waiting processes for incomming connection | integer | 5 |
| web_parallel_conns | The number of maximal parallel connections at the rest interface | integer | 50 |
| web_background_image | The path to a png image to use as background | file | default |
| ssl | Whether SSL should be used | boolean | true |
| cachain_file | Location of the ca chain for the server  | file | none |
| cert_file | Location of the certificate  | file | /etc/watts/watts.crt |
| key_file | Path to the private key file | file | /etc/watts/watts.key |
| dh_file | Path to the file containing the diffie hellman parameter. To generate it simply run `openssl dhparam -out watts_dh.pem 2048` | file | none |
| session_timeout | The duration for which a session at the web-app is valid during inactivity | duration | 15m |
| session_max_duration | The maximum duration of a session at the web interface, even if active. After this time an additional Login is required | duration | 30m |
| database_type | The type of database to user | 'sqlite', 'mnesia', 'eleveldb' | 'sqlite' |
| sqlite_file | Path to the sqlite database, used if database_type is 'sqlite' | file | /etc/watts/watts.db |
| mnesia_dir | Base directory of the mnesia database, used if database_type is 'mnesia' | dir | /var/lib/watts/mnesia |
| eleveldb_dir | Base directory of the eleveldb database, used if database_type is 'eleveldb' | dir | /var/lib/watts/eleveldb |
| redirection.enable | Whether redirection should be enabled | boolean | false |
| redirection.listen_port | The port to listen on for browsers to redirect | port | 8080 |
| secret_dir |  A directory to contain sensible secrets. WaTTS creates the directory if it does not yet exist and forces it to have the permissions 700 on it. | dir | /var/lib/watts/secret/ |
| jwt_key_rotation_interval | The interval at which the RSA keys for signing the session info (stored in cookies of the user) will be rotated. | duration |  14d |
| jwt_key_bits | The number of bits to use in the RSA key.  |  integer | 2014 |
| allow_dropping_credentials | Whether credentials of unknown services can be silently dropped | boolean | false |
| enable_user_doc | Whether the user documentation is reachable at /docs/user/ | boolean | true |
| enable_code_doc | Whether the code documentation is reachable at /docs/code/ | boolean | true |
| max_provider_wait | The duration to wait for provider results. If the duration is passed the results of OpenID Connect provider won't be logged, the provider will still function. | duration | 30s |
| log_dir | The path where the log files will be put | path | /var/log/watts |
| syslog_facility | The facility to use for syslog | 'daemon','local0'-'local7'  | 'daemon' |
| enable_rsp | Enable support for RSP (relying service provider) at WaTTS. This will enable support for other services to rely on WaTTS to perform certain taks e.g. run plugins for them. | boolean | false |
| privacy_doc | The html file containing the privacy statement for you WaTTS instance | file | none |
| debug_mode | enable debug output, this adds a lot of load. debug_mode is only allowed when running on localhost as it might log senstive data. | boolean  | false |
| max_error_msg_per_sec | The number of error messages that should be log at max, if there are more messages per second they will be dropped. If the number is negative nothing will be dropped. | integer | -1 |
| web_connection_rate | The number of connection per seccond that will be allowed to pass, every additional connection gets a '503 - service not available' available error. A negative value or zero changes it to unlimited, which should be used with caution. | integer | 10 |
| rsp_connection_rate | same as the web_connection_rate but for the rsp endpoint | integer | 10 |
| web_queue_max_wait | The maximum amount of time to wait until the request get cancelled while waiting in the queue | duration | 100ms |
| rsp_queue_max_wait | Same as web_queue_max_wait but for the rsp | duration | 1s |
| admin_mail | The email address of the administrator | string | none |
| email.enable | Whether email sending should be enabled in general | boolean | false |
| email.on_plugin_error | If a mail should be sent on issues with a plugin (only if email is enabled, see email.enable) | boolean | true |
| email.name | The sender name to use in the email | string | WaTTS |
| email.address | The sender address to use in the email | string | watts@<hostname> |
| email.relay | The mail server or the domain to use to send the mails | string | none |
| email.no_mx_lookups | disable lookups of mx records of the relay | boolean | false |
| email.port | The port of the mail server to connect to  | port | 25 (465 SSL) |
| email.ssl | Whether a ssl connection should be established (see also tls) | boolean | false |
| email.tls | Use tls for starting a secure connection  | 'never', 'if_available', 'always' | 'always' |
| email.user | username to use to authenticate at the server | string | none |
| email.password | password to authenticate with at the server | string | none |


## Listen_Port, Redirection Explained
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

## Example
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
