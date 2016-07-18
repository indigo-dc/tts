REPO = tts
REBAR = $(shell pwd)/rebar3


.PHONY: all cln clean eunit ct elvis compile sample_config cookie rel tar run package

all: compile

clean:
	$(REBAR) clean -a
        # this is needed for building packages
	rm -rf _build/default/plugins

cln:
	$(REBAR) clean -a

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
ifeq ($(OVERLAY_VARS),)
	$(REBAR) release --overlay_vars ./config/vars.config
else
        cat $(OVERLAY_VARS) > ./config/vars_pkg.config
        cat ./config/vars.config >> ./config/vars_pkg.config
	$(REBAR) release --overlay_vars ./config/vars_pkg.config
endif

run: cookie 
	$(REBAR) run 

install_deps:
	$(REBAR) install_deps


include package.mk
