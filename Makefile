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
	rm -rf _build/test
	rm -rf _book

eunit: check
	$(REBAR) do eunit -v, cover -v

ct: compile
	./utils/setup_ct.sh
	$(REBAR) do ct -v, cover -v

dialyzer:
	$(REBAR) dialyzer


jenkins_ct: compile
	./utils/setup_ct.sh
	$(REBAR) ct


tests: elvis
	$(REBAR) do cover -r, dialyzer, eunit -v
	./utils/setup_ct.sh
	$(REBAR) do ct -v, cover -v

elvis: check
	$(REBAR) lint

compile: check
	$(REBAR) compile
	# workaround to ensure syslog gets build
	cd _build/default/lib/syslog && ./rebar compile


docs:
	gitbook build
	rm -rf priv/docs
	mkdir -p priv/docs
	cp -r _book/* priv/docs

rel: compile
	cat ./config/vars.config > ./config/vars_gen.config
ifneq ($(OVERLAY_VARS),)
	cat $(OVERLAY_VARS) >> ./config/vars_gen.config
endif
	$(REBAR) release


run: rel
	if [ -f ~/.config/watts/watts.conf ] ; then \
		cp ~/.config/watts/watts.conf _build/default/rel/watts/etc/watts.conf ; \
	fi ;
	./_build/default/rel/watts/bin/watts console

install_deps:
	$(REBAR) install_deps

sample_config:
	./utils/install_sample_config


clean_package:
	rm -rf package
	rm -rf distdir

include package.mk
