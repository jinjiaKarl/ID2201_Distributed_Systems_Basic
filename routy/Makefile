.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -noshell

MODS = map \
	   dijkstra \
	   interfaces \
	   hist \
	   routy \
	   test

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump