#!/bin/bash
TEMP_PATH=/tmp/tts_common_test

rm -rf $TEMP_PATH
mkdir -p $TEMP_PATH
cd $TEMP_PATH
git clone https://github.com/indigo-dc/ttsc
cd ttsc
./utils/compile.sh
echo -n "TTSc version: " && ./ttsc --version

cd $TEMP_PATH
git clone https://github.com/indigo-dc/tts_plugin_info
