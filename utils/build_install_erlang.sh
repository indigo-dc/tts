#!/bin/bash
VERSION=20.0
cd /tmp
wget --progress=bar http://erlang.org/download/otp_src_$VERSION.tar.gz
tar -xzf otp_src_$VERSION.tar.gz
rm otp_src_$VERSION.tar.gz
cd otp_src_$VERSION
./configure --with-ssl --enable-builtin-zlib --enable-plain-emulator --without-wx --without-jinterface --without-odbc
make
sudo make install
cd ..
rm -rf otp_src_$VERSION
