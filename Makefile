REBAR = $(shell pwd)/rebar3
APP=oidcc

.PHONY: all clean clean_all eunit ct elvis compile sample_config rel tar run

all: compile

clean:
	$(REBAR) clean

clean_all:
	$(REBAR) clean -a

eunit:
	$(REBAR) eunit
	cp _build/test/cover/eunit.coverdata .

ct:
	$(REBAR) ct

elvis:
	$(REBAR) lint

compile:
	$(REBAR) compile

sample_config:
	./utils/install_sample_config

rel:
	./utils/gen_random_cookie
	$(REBAR) release 

tar:
	./utils/gen_random_cookie
	$(REBAR) as prod tar 
run:
	./utils/gen_random_cookie
	$(REBAR) run
