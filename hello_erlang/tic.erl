-module(tic).
-export([first/0, test/0]).

first() ->
    receive
        {tic, X} ->
            io:format("tic: ~w~n", [X]),
            second()
    end.

second() ->
    receive
        {tac, X} ->
            io:format("tac: ~w~n", [X]),
            last();
        {toe, X} ->
            io:format("toe: ~w~n", [X]),
            last()
    end.

last() ->
    receive
        X ->
            io:format("end: ~w~n", [X])
    end.

test() ->
    Pid = spawn(tic, first, []),
    Pid ! {toe, bar},
    Pid ! {tac, gurka},
    Pid ! {tic, foo},
    ok.

% output:
% > c(tic).
% {ok,tic}
% > tic:test().
% tic: foo
% ok
% toe: bar
% end: {tac,gurka}