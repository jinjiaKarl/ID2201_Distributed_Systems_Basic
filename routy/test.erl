-module(test).
-export([test_status/0]).

test_status() ->
    Machine = 'sweden@192.168.5.15',
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    r2 ! {add, stockholm, {r1, Machine}},

    Pid = spawn(fun() ->
            receive
                {status, Msg} ->
                    io:format("Receive status: ~p~n", [Msg])
                end
            end),
   r2 ! {status, Pid}. 