PROJECT = tts

DEPS = cowboy uuid econfig erlydtl jsx ejwt base64url uri oidcc eper esqlite

dep_ejwt = git https://github.com/indigo-dc-tokentranslation/ejwt master
dep_esqlite = git https://github.com/indigo-dc-tokentranslation/esqlite.git master
dep_base64url = git https://github.com/indigo-dc-tokentranslation/base64url.git master
dep_uri = git https://github.com/indigo-dc-tokentranslation/uri.git master
dep_oidcc = git https://github.com/indigo-dc-tokentranslation/oidcc.git master
include erlang.mk
