.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell
ERL2 = erl -name sweden2@192.168.5.15 -setcookie secret -connect_all false -noshell

USA = erl -name usa@192.168.5.15 -setcookie secret -connect_all false -noshell 
China = erl -name china@192.168.5.15 -setcookie secret -connect_all false -noshell 
Sweden = erl -name sweden@192.168.5.15 -setcookie secret -connect_all false -noshell 
World = erl -name world@192.168.5.15 -setcookie secret -connect_all false

MODS = map \
	   dijkstra \
	   interfaces \
	   hist \
	   routy \
	   test \
	   usa \
	   china \
	   sweden \
	   world

start: compile
	$(ERL) -s test start

test: compile
	$(ERL2) -s test test_route

start_usa: compile
	$(USA) -s usa start

start_china: compile
	$(China) -s china start

start_sweden: compile
	$(Sweden) -s sweden start

start_world: compile
	$(World) -s world start

compile: $(MODS:%=%.beam)

clean:
	rm -rf *.beam erl_crash.dump