# WaTTS - The INDIGO Token Translation Service
WaTTS is using the [OpenId Connect certified library oidcc](https://github.com/indigo-dc/oidcc).


## Purpose
WaTTS creates credentials for services that do not natively support OpenID Connect.
Such services are for example ssh, S3 storage, OpenNebula or certificate authorities, CAs.

WaTTS creates credentials for these services by using plugins. Plugins are scripts
that can be adapted by a site-administrator.

## Quickstart
WaTTS requires [Erlang](http://www.erlang.org/) OTP 18.1 or newer to run (due to a bug in the
cover compiler, OTP 18.3 or newer is needed to run the tests). For configuration, WaTTS uses
[cuttlefish](https://github.com/basho/cuttlefish), which needs to be installed in the Erlang
Runtime Sytem folder. Supporting scripts are in the utils directory.

### Installation
First, clone the repository and change in the cloned folder:
```
git clone https://github.com/indigo-dc/tts.git
cd tts
```
<!-- If you do not have Erlang installed and want a fast and easy setup run: -->
For fast and easy setup, execute:
```
./utils/prepare_system.sh
```
This will install Erlang and cuttlefish on the system.

Copy the sample configuration and modify it according to the requirements; the config is located in
`~/.config/watts` folder:
```
make sample_config
```
### Running the local WaTTS instance
Once the adjustments are made in the config, or to (re)start the WaTTS after
changing the configuration , execute:
```
make run
```
In browser, open local WaTTS instance on [localhost:8080](http://localhost:8080)

In order to stop WaTTS, type the following in the console
(the '.' at the end is important):
```
q().
```

The sample configuration includes a development [INDIGO Identity and Access Management (IAM)](https://github.com/indigo-iam/iam)
configuration (which only runs at _localhost:8080_), and a simple Info service.

To add more OpenId Connect Provider or services, edit the configuration file.
Modifying the configuration is described in the [Configuration Guide](https://indigo-dc.gitbooks.io/token-translation-service/content/config.html).


## Documentation
Further documentation for WaTTS can be found in the [WaTTS Documentation](https://www.gitbook.com/book/indigo-dc/token-translation-service/details)
GitBook.
