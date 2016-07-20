REPO = tts
REBAR = $(shell pwd)/rebar3


.PHONY: all cln clean eunit ct elvis compile sample_config cookie rel tar run package

all: compile

clean:
	$(REBAR) clean -a
	rm -rf _build/default/plugins

eunit:
	$(REBAR) do eunit,cover -v

ct:
	$(REBAR) do ct,cover -v

elvis:
	$(REBAR) lint

compile:
	$(REBAR) compile

sample_config:
	./utils/install_sample_config

cookie:
	./utils/gen_random_cookie

rel: cookie
	cat ./config/vars.config > ./config/vars_tmp.config
ifneq ($(OVERLAY_VARS),)
	cat $(OVERLAY_VARS) >> ./config/vars_tmp.config
endif
	$(REBAR) release --overlay_vars ./config/vars_tmp.config

run: rel 
	./_build/default/rel/tts/bin/tts console

install_deps:
	$(REBAR) install_deps


include package.mk
