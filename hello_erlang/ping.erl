-module(ping).
-export([ping/2]).

% (foo@192.168.5.15)2> ping:ping({pong, 'bar@192.168.5.15'}, "aaaaa").
% "aaaaa from Pong"

ping(Reg, Msg) ->
    Reg ! {self(), Msg},
    receive
        MsgR ->
            io:format("~s~n", [MsgR])
    end.