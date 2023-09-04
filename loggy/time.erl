% implement Lamport time
-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    max(Ti, Tj).

leq(Ti, Tj) ->
    case Ti =< Tj of
        true ->
            true;
        false ->
            false
    end.

% return a clock that keeps track of the timestamps of the last messages seen from each worker 
clock(Nodes) ->
    lists:foldl(fun(N, Acc) -> [{N, zero()} | Acc] end, [], Nodes).

% return a clock that has been updated
% given that we have received a log message from a node at a given time
update(Node, Time, Clock) ->
    lists:keysort(2, lists:keyreplace(Node, 1, Clock, {Node, Time})).

% is it safe to log an event that happened at a given time
% if time is less than the minimum time seen from all workers, then it is safe
safe(Time, Clock) ->
    [{ _, MinWorkerTime }| _] = Clock,
    leq(Time, MinWorkerTime).