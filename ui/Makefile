.PHONY: check all compile

all: compile

check:
	../utils/check_elm.sh

compile: check
	elm-make --warn --output tts_ui_elm.js src/Main.elm
	closure-compiler --js tts_ui_elm.js --js_output_file tts_ui.js \
		--compilation_level ADVANCED_OPTIMIZATIONS  --language_in ECMASCRIPT5
	mkdir -p static/js
	rm -f static/js/tts_ui.js
	ln -s ../../tts_ui.js static/js/tts_ui.js



install: compile
	cp index.html ../priv/http_static/
	cp tts_ui.js ../priv/http_static/js/
