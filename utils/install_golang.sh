#!/bin/bash
cd `dirname $0`
UTILS_DIR=`pwd`
GO=`which go`

echo "GO = $GO"
ls -al $GO
if [ "x" == "x$GO" ]; then
    # go not installed
    echo "go not installed"
    GODIR="/usr/local/go"
    mkdir -p $GODIR
    export PATH="/usr/local/go/bin:$PATH"

elif [ -L $GO ]; then
    echo "go is a symbolic link"
    # go installed using a symbolic link
    GOBINDIR=`dirname $GO`
    cd $GOBINDIR
    GO=`readlink -f go`
    echo "go points to $GO"
    GODIR=`dirname $GO`
    GODIR=`cd $GODIR/.. && pwd`

elif [ -f $GO ]; then
    # go installed using a normal file
    GODIR=`dirname $GO`
    GODIR=`cd $GODIR/.. && pwd`
fi
echo "GODIR = $GODIR"

cd $UTILS_DIR/..
mkdir -p _build/tmp/go
cd _build/tmp/go

wget --progress=bar https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz
tar -xzf go1.8.3.linux-amd64.tar.gz
cd go
sudo cp -r * $GODIR
cd ..
rm -rf go

echo -n "go version: "
go version
