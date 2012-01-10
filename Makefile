# Feel free to use, reuse and abuse the code in this file.

all: app

fast: 
	@./rebar compile skip_deps=true

app: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean:
	@./rebar clean
	rm -f erl_crash.dump

tests:
	@./rebar skip_deps=true eunit


dist-clean: clean
