#!/bin/bash
ERTS_VERISON=`erl -eval 'erlang:display(erlang:system_info(version)), halt().'  -noshell`
cd /tmp
git clone https://github.com/basho/cuttlefish.git
cd cuttlefish
git checkout -b 2.0.10
make
sudo cp cuttlefish  /usr/local/lib/erlang/erts-7.3/bin/
