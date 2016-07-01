# Deployment And Administration Guide 
## Installation
### From Package 
For Ubuntu 14.04 and CentOS 7 a package is provided by the INDIGO DataCloud
team.
To be able to install the packages using the package manager of your system the 
repository needs to be added. This is done by adding the INDIGO DataCloud
package repository to your system.

For informations on how to add the repository to your system refer to the
documentation at @TODO

#### Ubuntu 14.04 
After adding the repository one needs to update the package list and then install
the Token Translation Service.
```
apt update
apt install tts
```

#### CentOS 7
After adding the repository one needs to update the package list and then install
the Token Translation Service.
```
yum update
yum install tts
```

### From Source
To be able to install the Token Translation Service from source you need Erlang
OTP 18.1 or newer installed.

Prebuilt packages for your operation system should be ready at [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html).
Just download and install it.

Then follow these few steps:
- clone the git repository
- build the package yourself
- install the package
```
git clone https://github.com/indigo-dc/tts
cd tts
make clean
make packages
```
the package can be found in `./package/packages`.

## Configuration
### Basic Configuration
### Identity Harmonization (IDH)
### OpenId Connect Provider 
### Services 
