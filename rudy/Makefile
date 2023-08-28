.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -noshell

MODS = http \
	   rudy \
	   rudy_multi \
	   rudy_poll \
	   server \
	   test 

start_multi: compile
	${ERL} -eval 'server:start_multi(8080)'

stop_multi: compile
	${ERL} -eval 'server:stop_multi()'

start_poll: compile
	${ERL} -eval 'server:start_poll(8081)'

stop_poll: compile
	${ERL} -eval 'server:stop_poll()'

start: compile
	${ERL} -eval 'server:start(8082)'

start_bg: compile
	nohup ${ERL} -eval 'server:start(8082)' &

stop: compile
	${ERL} -eval 'server:stop()'

bench_multi: compile
	${ERL} -eval 'test:bench("127.0.0.1", 8080, 100)' -s init stop

bench_poll: compile
	${ERL} -eval 'test:bench("127.0.0.1", 8081, 100)' -s init stop

bench: compile
	${ERL} -eval 'test:bench("127.0.0.1", 8082, 100)' -s init stop

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump