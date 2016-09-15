.PHONY: check all compile

all: compile

check:
	../utils/check_elm.sh

compile: check
	elm-make --output tts_ui.js src/Main.elm


install: compile
	cp tts_ui.js ../priv/http_static/js
