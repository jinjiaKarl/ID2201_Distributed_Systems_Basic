-module(sweden).
-export([start/0, stop/0]).

start() ->
    io:format("Initiating stockholm, gothenburg ~n"),
    Machine = 'sweden@192.168.5.15',
    routy:start(s1, stockholm),
    routy:start(s2, gothenburg),
    timer:sleep(100),

    % stockholm <-> gothenburg
    s1 ! {add, gothenburg, {s2, Machine}},
    s2 ! {add, stockholm, {s1, Machine}},

    % broadcast link-state messages and update the map
    s1 ! broadcast,
    s2 ! broadcast,

    timer:sleep(100),
    s1 ! update,
    s2 ! update,
    ok.

stop() ->
    routy:stop(s1),
    routy:stop(s2).