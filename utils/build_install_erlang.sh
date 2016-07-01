#!/bin/bash
VERSION=18.3
cd /tmp
wget http://erlang.org/download/otp_src_$VERSION.tar.gz 
tar -xzf otp_src_$VERSION.tar.gz
rm otp_src_$VERSION.tar.gz
cd otp_src_$VERSION
./configure
make
sudo make install
cd ..
rm -rf otp_src_$VERSION
