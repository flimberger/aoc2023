.-include "local.mk"

all:	$(MOD).beam
clean:
	rm -f *.beam erl_crash.dump

$(MOD):	$(MOD).beam
	erl -boot start_clean -noshell -s $(MOD) run -s init stop
.PHONY:	all clean $(MOD)

.SUFFIXES:	.erl .beam
.erl.beam:
	erlc -W $<
