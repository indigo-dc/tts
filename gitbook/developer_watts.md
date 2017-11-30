# WaTTS Development

## Setting up the development environment
### Basic Setup
Setting up a basic development environment takes about 10-15 minutes, out of which 80%
is waiting for compilation.

The steps to perform are:
 - clone the git repository
 - run the prepare_system.sh script
 - start coding


 ```
 git clone git@github.com:indigo-dc/tts.git
 cd tts
 ./utils/prepare_system.sh
 ```

### Complete Setup
The basic setup is missing a few essential things:
- nodejs and npm to be able to install the later two
- the elm language, used for UI development
- a gitbook installation to generated the user documentation

#### Installing Npm
Elm and gitbook both use npm, so first we need to install nodejs (for debian/ubuntu):
```
apt-get install nodejs-legacy
```

Then we will build npm from source as it is not in the debian packages anymore:
```
git clone https://github.com/npm/npm.git
cd npm
git checkout v5.6.0
make
sudo make install
```

#### Installing Elm
Elm is a functional language that compiles to javascript. The benefit is
that once the compliler is satisfied you won't have runtime issues.

Elm needs npm, so please follow the instructions 'installing npm' first.
The only thing to do is installing elm with npm. WaTTS is currently built with version 0.18.0.
```
sudo npm install elm@0.18.0 -g
```

now building the ui is easy (in the WaTTS source directory):
```
make ui_install
```

#### Installing gitbook
The user documentation is generated using gitbook, which also is not in the debian packages.
Gitbook also uses npm, so please follow the 'installing npm' instructions above first.
Instlaling gitbook is then as easy as:
```
sudo npm install gitbook-cli@2.3.0 -g
```

Building the user documentation is now
```
make gitbook
```

## Directory overview
The WaTTS folder has different subfolder each for a different purpose:
 - config - as the name tells you, it contains the configuration files
 - config/ERLANG_VERSION - contains the erlang VM version to use during prepare
 - config/vars.config - the default variables to be set when building a release
 - config/watts.conf - the example config
 - config/schema - contains the schema files to parse and validate the configuration file
 - doc/ - contains the overview.edoc which is the main page for the code doc
 - docker - contains the Dockerfile to build a docker container (WIP)
 - gitbook - contains the gitbook markdown documentation, used to generate the user documentation
 - include - contains headerfiles for WaTTS
 - priv - the static data area, will be included in a release. The generated user and code documentation will be copied to here as well as the generated javascript ui.
 - priv/http_static/ includes the files neede for the web interface
 - src - contains all the WaTTS code
 - test - contains the unit and integration tests of WaTTS
 - ui - contains the ui configuration
 - ui/src - contains the ui source
 - utils - contains helper scripts to e.g. install needed depencencies

## Build targets
The makefile has multiple targets:
- all (default): compile WaTTS
- complile: compile WaTTS
- ui: build the user interface
- ui_install: build the user interface and include it in the priv directory
- check: check if erlang is installed
- clean: clean up the build structure
- elvis: perform codestyle checks
- eunit: run the unit tests
- ct: run the integration tests
- dialyzer: run the static code analysis tool
- tests: run elvis, eunit, ct and dialyzer
- edoc: generate code documentation and copy into priv directory
- gitbook: generate user documentation and copy into priv directory
- plt: generate the static code analysis of the Erlang system
- rel: create a release, that is a contained locally system
- run: run the release
- debug_config: start the config parser in debug mode to find issues with schema files
- install_deps: install needed 3rd party libraries
- sample_config: generate a sample config and copy it to ~/.config/watts/watts.conf
- clean_package: clean up the package building directories
- package: generate a package for the current system

## Upgrading 3rd party libraries
Rebar, the buildtool used through the Makefile, keeps track of the used libraries and
pins them to ensure that the release will always built the exact same way.

The pinning file is rebar.lock, so please ensure you also commit that!

Upgrading a library is done in two steps:
- upgrade the dependency in rebar.conf
- run `./rebar3 upgrade`

## Upgrading the used Erlang VM
The Erlang vm is fetched and compiled with the build_install_erlang.sh script (in utils).

The version is extracted from config/ERLANG_VERSION:
- VERSION is the release to use, currently 20.1
- ERTS is the erlang runtime version, that is usually the version number minus eleven
So if the version 20.1 is used the erts is 20.1 - 11 = 9.1
```
VERSION=20.1
ERTS=9.1
```

After changing the version the new erlang VM needs to be install on the system:
```
./utils/build_install_erlang.sh
```


## Building a package
The code to build a package will get the current checked out version WITHOUT local modification.
This is to ensure that packages are only built from source that is under version control.

Important is to check in the user documentation and the changed ui as those are not built
during package generation.

A typical package process looks like this:
```
git checkout v1.4.0
make clean_package
make package
```
