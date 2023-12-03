.-include "local.mk"

ENTRYPOINT ?= run

all:	$(MOD).beam
clean:
	rm -f *.beam erl_crash.dump

$(MOD):	$(MOD).beam
	erl -boot start_clean -noshell -s $(MOD) $(ENTRYPOINT) -s init stop
test:
	erl -boot start_clean -noshell -s $(MOD) test -s init stop
.PHONY:	all clean $(MOD)

.SUFFIXES:	.erl .beam
.erl.beam:
	erlc -W $<
