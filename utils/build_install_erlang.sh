#!/bin/bash
VERSION=20.0.4
cd /tmp

# http://erlang.org/download/otp_src_$VERSION.tar.gz
OTPNAME=OTP-$VERSION

rm -f $OTPNAME.tar.gz
wget --progress=bar https://github.com/erlang/otp/archive/$OTPNAME.tar.gz
tar -xzf $OTPNAME.tar.gz
rm $OTPNAME.tar.gz
cd otp-$OTPNAME
./otp_build autoconf
./configure --with-ssl --enable-builtin-zlib --without-wx --without-jinterface --without-odbc --without-debugger --without-observer --without-et
make
sudo make install
cd ..
rm -rf otp-$OTPNAME
