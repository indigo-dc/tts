PROJECT = tts

DEPS = cowboy uuid econfig erlydtl jsx ejwt base64url uri oidcc eper esqlite
BUILD_DEPS = elvis_mk

dep_ejwt = git https://github.com/indigo-dc-tokentranslation/ejwt cc9f769 
dep_esqlite = git https://github.com/mmzeeman/esqlite.git a0c5b39
dep_base64url = git https://github.com/dvv/base64url.git f2c64ed 
dep_uri = git https://github.com/erlware/uri.git 91f6b71
dep_oidcc = git https://github.com/indigo-dc-tokentranslation/oidcc.git 9cbba42 


dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 784e41b 

DEP_PLUGINS = elvis_mk

COVER = 1
include erlang.mk
