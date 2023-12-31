-module(loggy_lamport).
-export([start/1, stop/1]).

% OTP 21.0 introduced the logger module, so I change the module name to loggy.

% given a list of nodes that will send it messages
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    erlang:system_flag(max_heap_size, 10000),
    loop(time:clock(Nodes), []).

loop(Clock, HBQ) ->
    receive 
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedHBQ = lists:keysort(2, [{From, Time, Msg} | HBQ]),
            % iterate over the HBQ, if it is safe, then log the event from the HBQ
            % if it is not, add it back to the HBQ
            % io:format("HBQ: ~p, length: ~p Clock: ~p~n", [UpdatedHBQ, length(UpdatedHBQ), UpdatedClock]),
            NewHBQ = checkSafe(UpdatedClock, UpdatedHBQ, []),
            loop(UpdatedClock, NewHBQ);
        {holdback, Pid} ->
            % get status of the HBQ
            Pid ! {holdback, HBQ},
            loop(Clock, HBQ);
        stop ->
            HeapSize = erlang:system_info(max_heap_size),
            io:format("\nHoldback ~p~nSize of Holdback Queue: ~w~nClock ~p~nMax heap size: ~p~n", [HBQ, length(HBQ), Clock, HeapSize]),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

checkSafe(_, [], UnSafe) ->
    UnSafe;
checkSafe(Clock, [{From, Time, Msg} | Rest], UnSafe) ->
    case time:safe(Time, Clock) of
        true ->
            log(From, Time, Msg),
            checkSafe(Clock, Rest, UnSafe);
        false ->
            % because the HBQ and Clock are sorted, we can stop iterating once we find an unsafe event
            checkSafe(Clock, [], [{From, Time, Msg} | Rest])
    end.