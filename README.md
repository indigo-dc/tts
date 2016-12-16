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

The following lines clone the git repository and creates the default configuration
in `~/.config/tts` and installs a sample plugin, called 'info' into the same directory:
```
git clone https://github.com/indigo-dc/tts.git
cd tts
make sample_config
```
Now just fire up TTS:
```
make run
```
[Now Browse to local TTS on localhost:8080](http://localhost:8080)

If you want to stop the TTS you just need to type the following in the console
(the '.' at the end is important):
```
q().
```

The sample configuration includes a development [INDIGO Idetntity and Access Management (IAM)](https://github.com/indigo-iam/iam)
configuration that only runs at localhost:8080 and a simple Info service.

Just edit the configuration file to add more OpenId Connect Provider or services.
The steps needed to do this are described in the [Configuration Guide](https://indigo-dc.gitbooks.io/token-translation-service/content/config.html)


## Documentation
Further documentation of the TTS can be found at the [Token Translation Service
Documentation](https://www.gitbook.com/book/indigo-dc/token-translation-service/details)
on GitBook.
