REPO = $(shell basename $(shell pwd))
REBAR = $(shell pwd)/rebar3
MODULES = watts_init watts watts_app watts_sup watts_http_api watts_oidc_client \
		watts_persistent watts_plugin watts_plugin_runner watts_service \
		watts_service_authz watts_temp_cred watts_temp_cred_data watts_data


.PHONY: check all cln clean eunit ct elvis compile ui tests rel tar run package clean_package gitbook edoc callgraph

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
	rm -rf _build/default/rel
	rm -rf _build/test
	rm -rf _book

eunit: check
	$(REBAR) do eunit -v, cover -v

ct: compile
	./utils/setup_ct.sh
	$(REBAR) do ct -v, cover -v

jenkins_ct: compile
	./utils/setup_ct.sh
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

tests: elvis
	$(REBAR) do cover -r, eunit -v
	./utils/setup_ct.sh
	$(REBAR) do ct -v, cover -v, dialyzer

elvis: check
	$(REBAR) lint

compile: check
	$(REBAR) compile
	# workaround to ensure syslog gets build
	cd _build/default/lib/syslog && ./rebar compile

edoc:
	$(REBAR) edoc
	rm -rf priv/docs/code
	mkdir -p priv/docs/code
	cp -r doc/* priv/docs/code

gitbook:
	gitbook build
	rm -rf priv/docs/user
	mkdir -p priv/docs/user
	cp -r _book/* priv/docs/user
	rm -rf _book

plt:
	dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit ssl public_key

callgraph:
	dialyzer --dump_callgraph priv/docs/graphs/watts.dot src/*.erl; \
	dot priv/docs/graphs/watts.dot -Tpng -opriv/docs/graphs/watts.png; \


rel: compile edoc
	./utils/build_install_info_plugin.sh
	cat ./config/vars.config > ./config/vars_gen.config
ifneq ($(OVERLAY_VARS),)
	cat $(OVERLAY_VARS) >> ./config/vars_gen.config
endif
	$(REBAR) release


debug_config: rel
	if [ -f ~/.config/watts/watts.conf ] ; then \
		cp ~/.config/watts/watts.conf _build/default/rel/watts/etc/watts.conf ; \
	fi ;
	./_build/default/rel/watts/bin/watts config generate -l debug

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
