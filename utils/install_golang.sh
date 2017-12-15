#!/bin/bash
cd `dirname $0`
UTILS_DIR=`pwd`
GO=`which go`

if [ "x" != "x$GO" ] ; then
    echo "go already installed"
    exit 0
fi

cd $UTILS_DIR/..
mkdir -p _build/tmp/go
cd _build/tmp/go

curl -O https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz
tar -xzf go1.8.3.linux-amd64.tar.gz
sudo mv go /usr/local
cd ..
rm -rf go
export PATH="/usr/local/go/bin:$PATH"

echo -n "go version: "
go version
