# Complete Config Example
This example uses the info plugin, which is a very simple plugin and can be downloaded [here](https://github.com/indigo-dc/tts_plugin_info).
```
# only change the following two if
# you know what you are doing
nodename = watts@127.0.0.1
distributed_cookie = watts

# The basic settings
hostname = watts.example.com
port = 443
listen_port = 8443

# enable redirection from http to https
redirection.enable = true
redirection.listen_port = 8080

# make WaTTS run with SSL
ssl = true
cachain_file = /etc/watts/cert/ca-bundle.crt
cert_file = /etc/watts/cert/server.crt
key_file = /etc/watts/cert/server.key
dh_file = /etc/watts/cert/dh.pem

# for verification of the OpenID Connect provider
oidc.cacertfile = /etc/ssl/certs/ca-certificates.crt
oidc.cert_depth = 3

# enable the links to the documentation
# at the bottom of the page
enable_user_docs = true
enable_code_docs = true

# email settings
# this will result in a mail being sent to the admin at boot
admin_email = admin@company.com
email.enable = true
email.relay = company.com
email.user = watts_user
email.password = 123456

# the simple database settings to keep track of
# the credentials.
database_type = sqlite
sqlite_file = /home/watts/watts.db

# an OpenID Connect provider, INDIGO IAM
openid.iam.description = INDIGO Datacloud Identity and Access Management (IAM)
openid.iam.client_id = e70e7190-d64cad3771b9
openid.iam.client_secret = AN5pY379_Z_1jgiiAyKBEHe8zKP2KuiWxV34zWzw
openid.iam.config_endpoint = https://iam.indigo-datacloud.eu/.well-known/openid-configuration
openid.iam.request_scopes = openid, profile

# the simple info service
service.info.description = Simple Info Service
service.info.credential_limit = 1
service.info.connection.type = local
service.info.cmd = /home/watts/info.py
service.info.parallel_runner = infinite
service.info.authz.allow.any.sub.any = true
# send a mail on issues to the admin and the plugin developer
service.email_on_error_to = admin_devel
```
