#!/bin/bash
cd `dirname $0`
UTILS_DIR=`pwd`
GO=`which go`

echo "installing:"
echo "   _____  ____  _                   ";
echo "  / ____|/ __ \| |                  ";
echo " | |  __| |  | | | __ _ _ __   __ _ ";
echo " | | |_ | |  | | |/ _\` | '_ \ / _\` |";
echo " | |__| | |__| | | (_| | | | | (_| |";
echo "  \_____|\____/|_|\__,_|_| |_|\__, |";
echo "                               __/ |";
echo "                              |___/ ";

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
sudo rm -rf $GODIR/*


cd $UTILS_DIR/..
mkdir -p _build/tmp/go
cd _build/tmp/go
wget https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz
tar -xzf go1.8.3.linux-amd64.tar.gz
cd go
sudo cp -vr * $GODIR
cd ..
rm -rf go

GOROOT=$GODIR
export $GOROOT
echo -n "go version: "
go version
