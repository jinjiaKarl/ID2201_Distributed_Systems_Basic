.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell

MODS = loggy \
	   worker \
	   test

start: compile
	$(ERL) -s test start

test: compile
	$(ERL2) -s test test_route

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump