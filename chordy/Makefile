.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell

MODS = key \
	   storage \
       node1 \
       node2 \
       node3 \
	   test 


start: compile
	$(ERL) -s test start

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump