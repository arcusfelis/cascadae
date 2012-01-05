#!/bin/sh
erl -name cascadae -pa ../cascadae/ebin -pa deps/*/ebin  \
	-boot start_sasl -s cascadae 
