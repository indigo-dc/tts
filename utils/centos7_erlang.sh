#!/bin/bash

sudo yum update -y
sudo yum install -y wget ncurses-devel openssl-devel
sudo yum groupinstall -y "Development Tools"

./build_install_erlang.sh
