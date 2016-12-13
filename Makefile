REPO = tts
REBAR = $(shell pwd)/rebar3


.PHONY: check all cln clean eunit ct elvis compile ui tests rel tar run package clean_package

all: compile

ui:
	make -C ui

ui_install:
	make install -C ui

check:
	./utils/check_erlang.sh

clean: check
	$(REBAR) do cover -r, clean -a
	rm -rf _build/default/plugins
	rm -rf _build/default/bin

eunit: check
	$(REBAR) do eunit,cover -v

ct: check
	./utils/setup_ct.sh
	$(REBAR) do ct,cover -v

tests: elvis
	$(REBAR) cover -r
	$(REBAR) eunit
	./utils/setup_ct.sh
	$(REBAR) do ct,cover -v

elvis: check
	$(REBAR) lint

compile: check
	$(REBAR) compile

rel: check
	cat ./config/vars.config > ./config/vars_gen.config
ifneq ($(OVERLAY_VARS),)
	cat $(OVERLAY_VARS) >> ./config/vars_gen.config
endif
	$(REBAR) release


run: rel
	if [ -f ~/.config/tts/tts.conf ] ; then \
		cp ~/.config/tts/tts.conf _build/default/rel/tts/etc/tts.conf ; \
	fi ;
	./_build/default/rel/tts/bin/tts console

install_deps:
	$(REBAR) install_deps

sample_config: rel
	./utils/install_sample_config


clean_package:
	rm -rf package
	rm -rf distdir

include package.mk
