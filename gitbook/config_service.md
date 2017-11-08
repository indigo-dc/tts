# Services
## Introduction
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


## Settings
Each setting is prefixed with 'service.`id`.' where `id` must be replaced by the id
you want to give to the service.

| Key | Description | Datatype | Mandatory |
| :---: | --- | :---: | :---: |
| description | A description of the service for the user | string | yes |
| display_prio | the priority of the service in the listing. the lower the number the higher in the list the service will be listed. If the priority is 'undefined', the service is listed below the ones that have a numeric priority. | integer or 'undefined' | no ('undefined') |
| cmd | The command to execute after connecting, needs to be executable/readable by the user WaTTS is running as | string | yes |
| cmd_env_use | The parameter will not be passed at the command line but put into an environment variable. The default is 'false'. | boolean | no |
| cmd_env_var | The name of the environment variable containing the parameter, only used if `cmd_env_use` is true | string | no |
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
| email_on_error_to | Whom to send emails in case of plugin issues. Only if emails are enabled and mail on plugin errors are enabled (see server settings) | 'noone', 'admin', 'developer' or 'admin_devel' | no (noone) |




## Authorization
Authorization follows a few simple steps:
 1. at first everyone is forbidden
 2. if an `allow` rule that matches the user evaluates to true, she is allowed
 3. if a `forbid` rule that matches the user evaluates to true, he is forbidden

So a for a user to access a service she:
 - MUST match at least one `allow` rule
 - MUST NOT match any `forbid` rule

Each rule is exactly one line long. A rule always consists of five parts:
` authz.allow.p.k.o = v`, where the values are:

 - `p`: the Id of the provider, this can either be an OpenID Connect provider or an rsp
 - `k`: the key within the OpenId Connect Id-Token or user information
 - `info`: the information in the Id Token or user information with the key `k`
 - `o`: the operation to perform
 - `v`: the value

The provider Id for an OpenID Connect provider is the same as was given during the configuration,
in the provider example above the *id* was `iam`, so using that for `p` allows making decisions
on users coming from `iam`.

The provider Id for an RSP is the id of the RSP prefixed with an 'rsp-'. So the simple RSP in
the example above would have the *id* `rsp-simple`. All connections from an RSP get logged in
by the information sent by the RSP, an additional login at configured OpenID Connect providers
can be performed.

There is a special provider *id* value, `any`, which matches any provider, meaning any OpenID
Connect or RSP provider.

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


## Examples
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

## Complete Example
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
