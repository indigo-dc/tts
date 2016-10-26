#!/bin/bash
ELM_MAKE=`which elm-make`
CLOSURE_COMPILER=`which closure-compiler`
if [ "x$ELM_MAKE" == "x" ] ; then
    echo "***"
    echo " no Elm installed on the system"
    echo " please install Elm Version 0.17.0 "
    echo " "
    exit 1
elif [ "x$CLOSURE_COMPILER" == "x" ] ; then
    echo "***"
    echo " no closure-compiler installed on the system"
    echo " please install the closure compiler "
    echo " "
    exit 1
else
    echo "Elm and closure-compiler found on the system"
    exit 0
fi
