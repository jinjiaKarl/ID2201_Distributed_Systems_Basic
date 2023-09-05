-module(test).
-export([get_status/1, start/0, stop/0, test_route/0]).

-define(machine, 'sweden@192.168.5.15').

% topology
% +--------------+    +---------------+
% | stockholm(r1)+----+   lund(r2)    |
% +------+-------+    +---------------+
%        |
%        |
% +------+------+
% | malmo(r3)   |
% +------+------+
%        |
%        |
% +------+------+
% | uppsala(r4) |
% +------+------+
%        |
%        |
% +------+------+
% | goteborg(r5)|
% +-------------+

get_status(Reg) ->
    Machine = 'sweden@192.168.5.15',
    Pid = spawn(fun() ->
            receive
                {status, Msg} ->
                    io:format("Receive status: ~p~n", [Msg])
                end
            end),
   {Reg, Machine} ! {status, Pid}. 


start() ->
    % Machine = 'sweden@192.168.5.15',
    Machine = ?machine,
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, malmo),
    routy:start(r4, uppsala),
    routy:start(r5, goteborg),

    timer:sleep(100), % wait for the processes to be started

    % stockholm <-> lund
    r1 ! {add, lund, {r2, Machine}},
    r2 ! {add, stockholm, {r1, Machine}},

    % malmo <-> stockholm
    r1 ! {add, malmo, {r3, Machine}},
    r3 ! {add, stockholm, {r1, Machine}},

    % malmo <-> uppsala
    r3 ! {add, uppsala, {r4, Machine}},
    r4 ! {add, malmo, {r3, Machine}},

    % uppsala <-> goteborg
    r4 ! {add, goteborg, {r5, Machine}},
    r5 ! {add, uppsala, {r4, Machine}},

    % broadcast link-state messages and update the map
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast,


    timer:sleep(100), % wait for the link-state messages to be broadcasted {links....}
    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update,
    ok.

test_route() ->
    Machine = 'sweden@192.168.5.15',
    {r1, Machine} ! {route, uppsala, stockholm, "hello"},
    ok.

stop() ->
    routy:stop(r1),
    routy:stop(r2),
    routy:stop(r3),
    routy:stop(r4),
    routy:stop(r5).

