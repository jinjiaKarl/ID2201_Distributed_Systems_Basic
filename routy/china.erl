-module(china).
-export([start/0, stop/0]).

start() ->
    io:format("Initiating beijing, shanghai, guangzhou ~n"),
    Machine = 'china@192.168.5.15',
    routy:start(c1, beijing),
    routy:start(c2, shanghai),
    routy:start(c3, guangzhou),
    timer:sleep(100),

    c1 ! {add, shanghai, {c2, Machine}},
    c2 ! {add, beijing, {c1, Machine}},

    c2 ! {add, guangzhou, {c3, Machine}},
    c3 ! {add, shanghai, {c2, Machine}},

    c1 ! broadcast,
    c2 ! broadcast,
    c3 ! broadcast,
    timer:sleep(100),

    c1 ! update,
    c2 ! update,
    c3 ! update,
    ok.

stop() ->
    routy:stop(c1),
    routy:stop(c2),
    routy:stop(c3).

