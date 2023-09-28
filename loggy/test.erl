-module(test).
-export([run/2, run_lamport_no_q/2, run_lamport/2, run_lamport_hbq/2, run_vector/2]).

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

%  test:run_lamport(2000,1000).
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
% by default, the size of heap is no limit, https://www.erlang.org/doc/apps/erts/garbagecollection#sizing-the-heap
% run_lamport_hbq(20, 0).
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

run_vector(Sleep, Jitter) ->
    Logger = loggy_vect:start([john, paul, ringo, george]),
    A = worker_vect:start(john, Logger, 13, Sleep, Jitter),
    B = worker_vect:start(paul, Logger, 23, Sleep, Jitter),
    C = worker_vect:start(ringo, Logger, 36, Sleep, Jitter),
    D = worker_vect:start(george, Logger, 49, Sleep, Jitter),
    worker_vect:peers(A, [B, C, D]),
    worker_vect:peers(B, [A, C, D]),
    worker_vect:peers(C, [A, B, D]),
    worker_vect:peers(D, [A, B, C]),
    spawn(fun() ->
        timer:sleep(1000),
        E = worker_vect:start(jinjia, Logger, 60, Sleep, Jitter),
        worker_vect:peers(E, [A, B, C, D]),
        timer:sleep(1000),
        worker_vect:stop(E)
    end),
    timer:sleep(5000),
    loggy_vect:stop(Logger),
    worker_vect:stop(A),
    worker_vect:stop(B),
    worker_vect:stop(C),
    worker_vect:stop(D).
