#!/bin/bash
ELM_MAKE=`which elm-make`
if [ "x$ELM_MAKE" == "x" ] ; then
    echo "***"
    echo " no Elm installed on the system"
    echo " please install Elm Version 0.17.0 "
    echo " "
    exit 1
else
    echo "Elm found on the system"
    exit 0
fi
