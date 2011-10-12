all: compile

depends:
	@./rebar get-deps
	@./rebar update-deps

compile: build dialyze

clean:
	@./rebar clean

build:
	@./rebar compile

docs: compile
	@./rebar doc skip_deps=true

dialyze:
	@dialyzer ebin

build-plt:
	@dialyzer --build_plt --apps kernel stdlib compiler

test:
	@./rebar ct skip_deps=true

.PHONY: all depends compile clean build docs dialyze build-plt test
