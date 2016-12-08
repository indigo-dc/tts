.PHONY: check all compile

all: compile

check:
	../utils/check_elm.sh

compile: check
	elm-make --yes --warn --output tts_ui_elm.js src/Main.elm
	closure-compiler --js tts_ui_elm.js --js_output_file tts_ui.js \
		--compilation_level ADVANCED_OPTIMIZATIONS  --language_in ECMASCRIPT5


install: compile
	cp tts_ui.js ../priv/http_static/js/
	cp tts_ui.js ../_build/default/lib/tts/priv/http_static/js/
	rm -rf ../_build/default/rel
