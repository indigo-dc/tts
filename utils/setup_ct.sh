#!/bin/bash
TEMP_PATH=/tmp/watts_common_test

rm -rf $TEMP_PATH
mkdir -p $TEMP_PATH
cd $TEMP_PATH
git clone https://github.com/indigo-dc/wattson
cd wattson
./utils/compile.sh
echo -n "wattson version: " && ./wattson --version

cd $TEMP_PATH
git clone https://github.com/indigo-dc/tts_plugin_info
