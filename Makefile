PROJECT = tddreloader

# dependencies
#
DEPS = 

shell:
	erl -pa ebin -s tddreloader start

include erlang.mk
