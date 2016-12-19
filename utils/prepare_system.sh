#!/bin/bash
DISTRIBUTION_NAME=`cat /etc/os-release | grep PRETTY_NAME`
CURRENT_DIR=`pwd`
cd `dirname $0`
UTILS_DIR=`pwd`

DISTRIBUTION="unknown"
case "$DISTRIBUTION_NAME" in
    *Debian*)
        DISTRIBUTION="debian"
        ;;
    *Ubuntu*)
        DISTRIBUTION="ubuntu"
        ;;
    *CentOS*)
        DISTRIBUTION="centos"
        ;;
esac
echo "preparing the system ..."
echo "distribution: $DISTRIBUTION"
echo "utils dir: $UTILS_DIR"
echo "current dir: $CURRENT_DIR"
if [ "$DISTRIBUTION" = "unknown" ]; then
    echo "ERROR: unknown distribution"
    exit 1
fi

echo " "
echo " "
echo "*** INSTALLING PACKAGES ***"
cd $UTILS_DIR
case "$DISTRIBUTION" in
    debian)
        ./debian_install_packages.sh
        ;;
    ubuntu)
        ./ubuntu_install_packages.sh
        ;;
    centos)
        ./centos_install_packages.sh
        ;;
esac

echo " "
echo " "
echo "*** BUILDING AND INSTALLING ERLANG ***"
cd $UTILS_DIR
./build_install_erlang.sh

echo " "
echo " "
echo "*** BUILDING AND INSTALLING CUTTLEFISH ***"
cd $UTILS_DIR
./build_install_cuttlefish.sh

cd $CURRENT_DIR
echo " "
echo " "
echo "*** SYSTEM SETUP DONE ***"
echo " "
