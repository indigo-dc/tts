#!/bin/bash
cd `dirname $0`
UTILS_DIR=`pwd`
source $UTILS_DIR/../config/ERLANG_VERSION

cd /tmp

# http://erlang.org/download/otp_src_$VERSION.tar.gz
OTPNAME=OTP-$VERSION

rm -f $OTPNAME.tar.gz
wget --progress=bar https://github.com/erlang/otp/archive/$OTPNAME.tar.gz
tar -xzf $OTPNAME.tar.gz
rm $OTPNAME.tar.gz
cd otp-$OTPNAME
./otp_build autoconf
# plain emulator needed for cuttlefish
./configure --with-ssl --enable-builtin-zlib --without-wx --without-jinterface --without-odbc --without-debugger --without-observer --without-et --enable-plain-emulator
make
sudo make install
cd ..
rm -rf otp-$OTPNAME

echo " "
echo " "
echo "*** INSTALLING PREBUILT CUTTLEFISH ***"
cd $UTILS_DIR
sudo cp cuttlefish  /usr/local/lib/erlang/erts-$ERTS/bin/
sudo chmod 555 /usr/local/lib/erlang/erts-$ERTS/bin/cuttlefish
