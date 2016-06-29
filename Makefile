REPO			?= tts
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
OVERLAY_VARS    ?=
REBAR = $(BASE_DIR)/rebar3


$(if $(ERLANG_BIN),,$(warning "Warning: No Erlang found in your path, this will probably not work"))



.PHONY: all clean clean_all eunit ct elvis compile sample_config cookie rel tar run package

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
	$(REBAR) release

tar: cookie
	$(REBAR) as prod tar

run: cookie
	$(REBAR) run

deb: tar
	./packages/deb/build

rpm: tar
	./packages/rpm/build

install_deps:
	$(REBAR) install_deps


include package.mk
