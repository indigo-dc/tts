#!/bin/bash
ERL=`which erl`
if [ "x$ERL" == "x" ] ; then
    echo "***"
    echo " no Erlang installed on the system"
    echo " please install Erlang Version 18.3 or newer "
    echo " you can use the script utils/build_install_erlang.sh "
    echo " to install it from source, yet you need the build utils "
    echo " for your system installed. "
    echo " "
    exit 1
else
    echo "Erlang found on the system"
    exit 0
fi
    
