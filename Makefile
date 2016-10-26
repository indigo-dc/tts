REPO = tts
REBAR = $(shell pwd)/rebar3


.PHONY: check all cln clean eunit ct elvis compile ui tests sample_config cookie rel tar run package clean_package

all: compile

ui:
	make -C ui

ui_install: ui
	make install -C ui

check:
	./utils/check_erlang.sh

clean: check
	$(REBAR) do clean -a
	rm -rf _build/default/plugins
	rm -rf _build/bin/

eunit: check
	$(REBAR) do eunit,cover -v

ct: check
	$(REBAR) do ct,cover -v

tests: check
	$(REBAR) do eunit, ct, cover -v

elvis: check
	$(REBAR) lint

compile: check
	$(REBAR) compile

sample_config:
	./utils/install_sample_config

cookie:
	./utils/gen_random_cookie

rel: cookie check
	cat ./config/vars.config > ./config/vars_gen.config
ifneq ($(OVERLAY_VARS),)
	cat $(OVERLAY_VARS) >> ./config/vars_gen.config
endif
	$(REBAR) release


run: rel
	./_build/default/rel/tts/bin/tts console

install_deps:
	$(REBAR) install_deps

clean_package:
	rm -rf package
	rm -rf distdir

include package.mk
