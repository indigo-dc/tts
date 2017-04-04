# Deployment And Administration Guide
## Installation
### From Package
For Ubuntu 14.04 and CentOS 7 a package is provided by the INDIGO DataCloud
team.
In order to install the packages using the system package manager, the
repository needs to be added. This is done by adding the INDIGO DataCloud
package repository to your system.

The INDIGO DataCloud repository can be found at [INDIGO-DataCloud Software
Repository](http://repo.indigo-datacloud.eu).


For information how to add the repository to your system, please refer to the
documentation of your respective operating system.

#### Ubuntu 14.04
After adding the repository, one needs to update the package list and then install
the Token Translation Service.
```
apt update
apt install tts
```

#### CentOS 7
After adding the repository, one needs to update the package list and then install
the Token Translation Service.
```
yum update
yum install tts
```

### From Source
To be able to install the Token Translation Service from source, you need Erlang
OTP 18.1 or newer installed.

Pre-built package for your operating system can be found at [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).
Just download and install it.

Then follow the next few steps:

- clone the git repository
- build the package yourself
- install the package
```
git clone https://github.com/indigo-dc/tts
cd tts
make clean
make package
```

The package can be found in `./package/packages`.

## Upgrade WaTTS
### v1.0.0 to v1.1.0
Only two new settings are needed in the watts.conf:
- nodename: `nodename = watts@127.0.0.1`
- distributed_cookie: `distributed_cookie = watts`

Add these settings to your watts.conf and then restart WaTTs.

### v0.4.1 to v1.0.0
Upgrading from TTS to WaTTS is **not** straightforward as both the configuration
file and the plugin API have changed.
You can not reuse any old configuration or plugins.
<!-- Please see this Documentation on further information regarding a fresh setup. -->

For a clean and fresh setup, do the following:

* stop the TTS, with `tts stop`
* install the new package (still called `tts`)
* configure WaTTS according to your needs (see below for reusing existing data)
* start WaTTS, with `watts start`

If you really need to use the existing data, ensure the following:

* a plugin using the same data does exist for WaTTS 1.0
* you configure your newly set up WaTTS to use the existing *sqlite db*
* you configure the plugin to use the existing data

### v0.2.2 to v0.4.0 (called TTS)
Upgrading the TTS from version 0.2.2 to 0.4.0 is straightforward.
As the configuration files are compatible the only actions to do are:

* stop the TTS `tts stop`
* install the new package
* start the newly installed TTS `tts start`
