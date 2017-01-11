.PHONY: check all compile

all: compile

check:
	../utils/check_elm.sh

compile: check
	elm-make --yes --warn --output watts_ui_elm.js src/Main.elm
	closure-compiler --js watts_ui_elm.js --js_output_file watts_ui.js \
		--compilation_level ADVANCED_OPTIMIZATIONS  --language_in ECMASCRIPT5


install: compile
	cp watts_ui.js ../priv/http_static/js/
	cp watts_ui.js ../_build/default/lib/watts/priv/http_static/js/
	rm -rf ../_build/default/rel
