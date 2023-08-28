-module(server).
-export([start/1, stop/0, start_multi/1, stop_multi/0, start_poll/1, stop_poll/0]).

start(Port) ->
    register(rudy, spawn(fun() -> rudy:init(Port) end)).

stop() -> exit(whereis(rudy), "time to die").

start_multi(Port) ->
    register(rudy_multi, spawn(fun() -> rudy_multi:init(Port) end)).

stop_multi() -> exit(whereis(rudy_multi), "time to die").

start_poll(Port) ->
    register(rudy_poll, spawn(fun() -> rudy_poll:init(Port) end)).

stop_poll() -> exit(whereis(rudy_poll), "time to die").