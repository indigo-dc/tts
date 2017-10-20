# Relying Service Providers
## Introduction
A relying service provider (RSP) is a service that relys on WaTTS for either performing some
task or even completely for authentication.

## Settings
Each setting is prefixed with 'rsp.`id`.' where `id` must be replaced by the id
you want to give to the relying service provider.

| Key | Description | Datatype | Mandatory (Default) |
| :---: | --- | :---: | :---: |
| keys_location  | the location of the file / web-page containing the json object with the public keys (see below). the location must be a url with either 'https://' or 'file://' scheme. | url | yes |
| show_ui | if set to 'false' no user-interface will be shown. | boolean | no (true) |
| perform_login | If set to 'false' the user won't be redirected to a login. | boolean | no (true) |
| base_url | The url each url passed to WaTTS in the JWT must start with. | string | yes |



## Example
An example for an RSP, setting its id to `simple`:
```
rsp.simple.keys_location = https://simple-rsp.kit.edu/jwk
rsp.simple.perform_login = false
rsp.simple.show_ui = true
rsp.simple.base_url = https://simple-rsp.kit.edu
```
