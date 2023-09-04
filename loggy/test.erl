-module(test).
-export([run/2, run_lamport_no_q/2, run_lamport/2, run_lamport_hbq/2]).

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

run_lamport_no_q(Sleep, Jitter) ->
    Logger = loggy:start([john, paul, ringo, george]),
    A = worker_lamport:start(john, Logger, 13, Sleep, Jitter),
    B = worker_lamport:start(paul, Logger, 23, Sleep, Jitter),
    C = worker_lamport:start(ringo, Logger, 36, Sleep, Jitter),
    D = worker_lamport:start(george, Logger, 49, Sleep, Jitter),
    worker_lamport:peers(A, [B, C, D]),
    worker_lamport:peers(B, [A, C, D]),
    worker_lamport:peers(C, [A, B, D]),
    worker_lamport:peers(D, [A, B, C]),
    timer:sleep(5000),
    loggy:stop(Logger),
    worker_lamport:stop(A),
    worker_lamport:stop(B),
    worker_lamport:stop(C),
    worker_lamport:stop(D).

run_lamport(Sleep, Jitter) ->
    Logger = loggy_lamport:start([john, paul, ringo, george]),
    A = worker_lamport:start(john, Logger, 13, Sleep, Jitter),
    B = worker_lamport:start(paul, Logger, 23, Sleep, Jitter),
    C = worker_lamport:start(ringo, Logger, 36, Sleep, Jitter),
    D = worker_lamport:start(george, Logger, 49, Sleep, Jitter),
    worker_lamport:peers(A, [B, C, D]),
    worker_lamport:peers(B, [A, C, D]),
    worker_lamport:peers(C, [A, B, D]),
    worker_lamport:peers(D, [A, B, C]),
    timer:sleep(5000),
    loggy_lamport:stop(Logger),
    worker_lamport:stop(A),
    worker_lamport:stop(B),
    worker_lamport:stop(C),
    worker_lamport:stop(D).

% How large will the holdback queue be, make some tests and try to find the maximum number of entries.
% the maximum number of entries depends on the  memory of the system and whether you're using Erlang/OTP 32 or 64 bit.
% https://stackoverflow.com/questions/43864049/maximum-length-limitation-in-list-in-erlang
% run_lamport_hbq(20, 10).
run_lamport_hbq(Sleep, Jitter) ->
    Logger = loggy_lamport:start([john, paul, ringo, george]),
    % if georage is not in the peers list, then the length HBQ will always increase.
    A = worker_lamport:start(john, Logger, 13, Sleep, Jitter),
    B = worker_lamport:start(paul, Logger, 23, Sleep, Jitter),
    C = worker_lamport:start(ringo, Logger, 36, Sleep, Jitter),
    worker_lamport:peers(A, [B, C]),
    worker_lamport:peers(B, [A, C]),
    worker_lamport:peers(C, [A, B]),
    timer:sleep(5000),
    Pid = spawn(fun() ->
        receive
            {holdback, HBQ} ->
                io:format("HBQ: ~p, length: ~p~n", [HBQ, length(HBQ)])
        end
    end),
    Logger ! {holdback, Pid},
    timer:sleep(100),
    loggy_lamport:stop(Logger),
    worker_lamport:stop(A),
    worker_lamport:stop(B),
    worker_lamport:stop(C).