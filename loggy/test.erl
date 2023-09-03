-module(test).
-export([run/2]).

% Sleep: milliseconds, the maximum time a worker process to send a message to one of its peer
% Jitter: milliseconds, delay between sending the message to the peer and informing the logger.
% run(2000, 1000).
run(Sleep, Jitter) ->
    Logger = loggy:start([john, paul, ringo, george]),
    A = worker:start(john, Logger, 13, Sleep, Jitter),
    B = worker:start(paul, Logger, 23, Sleep, Jitter),
    C = worker:start(ringo, Logger, 36, Sleep, Jitter),
    D = worker:start(george, Logger, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(5000),
    loggy:stop(Logger),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).