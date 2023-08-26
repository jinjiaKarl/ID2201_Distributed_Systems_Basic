-module(pong).
-export([start/0, pong/0]).

% c(pong).
% pong:start().

start() ->
    register(pong, spawn(pong, pong, [])).

pong() ->
    receive
        {From, Msg} ->
            From ! string:concat(Msg, " from Pong") 
    end,
    pong().
