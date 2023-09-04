-module(loggy).
-export([start/1, stop/1]).

% OTP 21.0 introduced the logger module, so I change the module name to loggy.

% given a list of nodes that will send it messages
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(_) ->
    loop().

loop() ->
    receive 
        {log, From, Time, Msg} ->
            log(From, Time, Msg),
            loop();
        stop ->
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).