-module(wait).
-export([hello/0]).

% shell
% > c(wait).
% > Pid = spawn(wait, hello, []).
% > Pid ! "hello".
% or
% > register(foo, Pid).
% > foo ! "hello".
hello() ->
    receive
        X -> io:format("aaa! surprise, a message: ~s~n", [X])
    end.