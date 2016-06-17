REBAR = $(shell pwd)/rebar3
APP=oidcc

.PHONY: all clean clean_all eunit ct elvis compile sample_config cookie rel tar run

all: compile

clean:
	$(REBAR) clean

clean_all:
	$(REBAR) clean -a

eunit:
	$(REBAR) do eunit,cover -v

ct:
	$(REBAR) ct

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
