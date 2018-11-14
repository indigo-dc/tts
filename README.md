# WaTTS - The INDIGO Token Translation Service

WaTTS allows using any legacy service with federated identities, such as
eduGain or google.

For this, WaTTS accepts federated identities (via OpenID Connect) and uses
a plugin scheme to generate credentials for your service. This allows you
to provide services that do not normally support federated identities to
federated users.

And the best is that plugins for common services already exist. They can
be used as examples to support additional services.

Existing plugins are available for 
- SSH, SSH-CA
- S3 storage (commercial DDN appliance)
- OpenNebula cloud middleware
- X.509 online certificate authorities (CAs)

WaTTS uses the [OpenId Connect certified library oidcc](https://github.com/indigo-dc/oidcc).

## Quickstart
### Using Releases
WaTTS provides packages for every release, just download the package for your sytem from
the [latest release](https://github.com/indigo-dc/tts/releases/latest) and install them.
After installing you need to configure it according to the [Documentation](https://indigo-dc.gitbooks.io/token-translation-service/content/config.html).

### Using Nightly Builds
WaTTS has nightly builds for packages for
[centos](https://jenkins.indigo-datacloud.eu:8080/job/tts-packaging/platform=bcentos7/) and
[Ubuntu/Debian](https://jenkins.indigo-datacloud.eu:8080/job/tts-packaging/platform=bubuntu14/).
Download and install the package on your system and start configuring and using it, following
the [Documentation](https://indigo-dc.gitbooks.io/token-translation-service/content/config.html).

### Using Source
WaTTS requires [Erlang](http://www.erlang.org/) OTP 18.1 or newer to run (due to a bug in the
cover compiler, OTP 18.3 or newer is needed to run the tests). For configuration, WaTTS uses
[cuttlefish](https://github.com/basho/cuttlefish), which needs to be installed in the Erlang
Runtime Sytem folder. Supporting scripts are in the utils directory.

#### Installation
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
#### Running the local WaTTS instance
Once the adjustments are made in the config, or to (re)start the WaTTS after
changing the configuration , execute:
```
make run
```
In browser, open local WaTTS instance on [localhost:8080](http://localhost:8080).
The documentation is at [localhost:8080/docs/](http://localhost:8080/docs/).


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
Further documentation for WaTTS can be found in the
[WaTTS Documentation](https://www.gitbook.com/book/indigo-dc/token-translation-service/details)
GitBook.
