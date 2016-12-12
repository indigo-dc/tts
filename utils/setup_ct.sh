#!/bin/bash
TEMP_PATH=/tmp/tts_common_test

rm -rf $TEMP_PATH
mkdir -p $TEMP_PATH
cd $TEMP_PATH
git clone https://github.com/indigo-dc/ttsc
cd ttsc
make compile

cd $TEMP_PATH
git clone https://github.com/indigo-dc/tts_plugin_info
