# WaTTS - The INDIGO Token Translation Service
WaTTS is using the [OpenId Connect certified library oidcc](https://github.com/indigo-dc/oidcc).


## Purpose
WaTTS creates credentials for services that do not natively support OpenID Connect.
Such services are for example ssh, S3 storage, OpenNebula or certificate authorities, CAs.

WaTTS creates credentials for these services by using plugins. Plugins are scripts
that can be adapted by a site-administrator.

## Quickstart
WaTTS requires [Erlang](http://www.erlang.org/) OTP 18.1 or newer to run (due to a bug in the
cover compiler OTP 18.3 or newer is needed to run the tests). For configuration WaTTS uses
[cuttlefish](https://github.com/basho/cuttlefish), which needs to be installed into the Erlang
Runtime Sytem directory. Scripts to support you are in the utils directory of this repository.

First clone the repository and change into the cloned directory:
```
git clone https://github.com/indigo-dc/tts.git
cd tts
```
If you do not have Erlang installed and want a fast and easy setup run:
```
./utils/prepare_system.sh
```
This will install Erlang and cuttlefish onto you system.

Now copy the sample configuration and after that adjust it to your needs, the config will be at
`~/.config/watts`:
```
make sample_config
```

Once you are satisfied with your config or just to start TTS after some configuration changes:
```
make run
```
[Now Browse to your local WaTTS on localhost:8080](http://localhost:8080)

If you want to stop WaTTS you just need to type the following in the console
(the '.' at the end is important):
```
q().
```

The sample configuration includes a development [INDIGO Idetntity and Access Management (IAM)](https://github.com/indigo-iam/iam)
configuration that only runs at localhost:8080 and a simple Info service.

Just edit the configuration file to add more OpenId Connect Provider or services.
The steps needed to do this are described in the [Configuration Guide](https://indigo-dc.gitbooks.io/token-translation-service/content/config.html)


## Documentation
Further documentation of WaTTS can be found at the [WaTTS Documentation](https://www.gitbook.com/book/indigo-dc/token-translation-service/details)
on GitBook.
