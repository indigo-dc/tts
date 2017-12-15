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
    GO="/usr/local/bin/go"
else
    echo "go is a symbolic link or a normal file"
    # go installed using a symbolic link
    GOBINDIR=`dirname $GO`
    mv $GOBINDIR/go $GOBINDIR/go.old
fi

# delete maybe old go
mkdir -p /usr/local/
sudo rm -rf /usr/local/go

# download and unpack go
cd $UTILS_DIR/..
mkdir -p _build/tmp/go
cd _build/tmp/go
wget -q https://storage.googleapis.com/golang/go1.8.3.linux-amd64.tar.gz
tar -xzf go1.8.3.linux-amd64.tar.gz
sudo cp -r go /usr/local/go
rm -rf go

# link to newly installed go
ln -s /usr/local/go/bin/go $GO

echo -n "go version: "
go version
