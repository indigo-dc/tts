# Token Translation Service 

## Purpose
The Token Translation Service (TTS) is creating credentials for services that do
not natively support OpenID Connect. Services that do not support OpenID Connect
are for example ssh, S3 storage, OpenNebula.

The TTS creates these by using so called plugins, which are scripts managed by
the administrator of the TTS. An example plugin can is available 
[here](https://github.com/indigo-dc/tts/blob/master/tts_config/sample/tts/scripts/ssh.py)

## Quickstart
The Token Translation Service (TTS) requires Erlang OTP 18.1 or newer to run 
(due to a bug in the cover compiler OTP 18.3 or newer is needed to run the tests)

The following lines copy the [sample configuration](https://github.com/indigo-dc/tts/tree/master/tts_config/sample) 
to `~/.config/tts`, build the TTS and run it. 
```
git clone https://github.com/indigo-dc/tts.git
cd tts
make sample_config
make run 
```
The TTS starts on localhost at port 8080 and includes two OpenID Connect 
Provider, Google and IAM. 

[Browse to local TTS](http://localhost:8080)
