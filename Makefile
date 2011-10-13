all: compile

depends:
	@./rebar get-deps
	@./rebar update-deps

clean:
	@./rebar clean

compile:
	@./rebar compile

docs: compile
	@./rebar doc skip_deps=true

test:
	@./rebar ct skip_deps=true

.PHONY: all depends compile clean docs test
