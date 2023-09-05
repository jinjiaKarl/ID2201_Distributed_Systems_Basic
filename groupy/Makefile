.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell

MODS = gui \
	   worker \
	   test 


start: compile
	$(ERL) -s test start

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump