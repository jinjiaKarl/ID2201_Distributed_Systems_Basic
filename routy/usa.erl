-module(usa).
-export([start/0, stop/0]).

start() ->
    io:format("Initiating losangeles, newyork ~n"),
    Machine = 'usa@192.168.5.15',
    routy:start(u1, newyork),
    routy:start(u2, losangeles),
    timer:sleep(100),

    u1 ! {add, losangeles, {u2, Machine}},
    u2 ! {add, newyork, {u1, Machine}},

    u1 ! broadcast,
    u2 ! broadcast,
    timer:sleep(100),

    u1 ! update,
    u2 ! update,
    ok.

stop() ->
    routy:stop(u1),
    routy:stop(u2).
