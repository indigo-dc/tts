# Token Translation Service

## Purpose
The Token Translation Service (TTS) creates credentials for services that do
not natively support OpenID Connect. Such services are for example ssh, S3 storage,
OpenNebula or certificate authorities, CAs.

The TTS creates credentials for these services by using plugins. Plugins are scripts
that can be adapted by a site-administrator.

## Quickstart
The Token Translation Service (TTS) requires [Erlang](http://www.erlang.org/) OTP 18.1 or newer to run
(due to a bug in the cover compiler OTP 18.3 or newer is needed to run the tests)

The following lines copy the default configuration
to `~/.config/tts`, build the TTS and runs it.
```
git clone https://github.com/indigo-dc/tts.git
cd tts
make sample_config
make run
```
The TTS starts on localhost at port 8080 and includes one OpenID Connect
Provider, the INDIGO Identity and Access Managent (IAM) [IAM on Github](https://github.com/indigo-iam/iam)

[Browse to local TTS](http://localhost:8080)

If you want to stop the TTS you just need to type the following in the console
(the '.' at the end is important):
```
q().
```

## Documentation
Further documentation of the TTS can be found at the [Token Translation Service
Documentation](https://www.gitbook.com/book/indigo-dc/token-translation-service/details)
on GitBook.
