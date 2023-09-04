.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell

MODS = loggy \
	   loggy_lamport \
	   loggy_vect \
	   worker \
	   worker_lamport \
	   worker_vect \
	   time \
	   vect \
	   test

start: compile
	$(ERL) -eval "test:run_lamport(2000, 1000)"

test: compile
	$(ERL2) -s test test_route

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump