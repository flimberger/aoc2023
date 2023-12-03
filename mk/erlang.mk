.-include "local.mk"

ERL := erl -boot start_clean -noshell

all:	$(MOD).beam
clean:
	rm -f *.beam erl_crash.dump

puzzle1: $(MOD).beam
	$(ERL) -s $(MOD) puzzle1 -s init stop
test1: $(MOD).beam
	$(ERL) -s $(MOD) test1 -s init stop
puzzle2: $(MOD).beam
	$(ERL) -s $(MOD) puzzle2 -s init stop
test2: $(MOD).beam
	$(ERL) -s $(MOD) test2 -s init stop
.PHONY:	all clean puzzle1 test1 puzzle2 test2

.SUFFIXES:	.erl .beam
.erl.beam:
	erlc -W $<
