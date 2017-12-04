#!/bin/bash
cd `dirname $0`
cd ..
REPO_DIR=`pwd`

mkdir -p _build/tmp
mkdir -p _build/plugins
if [ -e _build/plugins/info_plugin ]
then
    echo "info plugin present, won't rebuild"
    exit 0
fi

cd _build/tmp
git clone https://github.com/watts-kit/watts_plugin_info.git
cd watts_plugin_info
./utils/compile.sh
cd ../..
cp tmp/watts_plugin_info/watts_plugin_info plugins/info_plugin
rm -rf tmp/

cd $REPO_DIR
