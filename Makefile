PROJECT = tts

DEPS = cowboy uuid econfig erlydtl oidcc eper esqlite lager
BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk
TEST_DEPS = meck

dep_oidcc = git https://github.com/indigo-dc/oidcc.git 1aded8e

dep_esqlite = git https://github.com/mmzeeman/esqlite.git a0c5b39
dep_base64url = git https://github.com/dvv/base64url.git f2c64ed 
dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_lager = git https://github.com/basho/lager.git 2.2.2
# dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 215616a



COVER = 1
include erlang.mk
