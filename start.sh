#!/bin/sh
erl -sname cowboy_examples -pa ebin -pa deps/*/ebin \
	-boot start_sasl -s cascadae 
