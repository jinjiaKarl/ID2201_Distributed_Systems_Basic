-module(loggy_vect).
-export([start/1, stop/1]).

% OTP 21.0 introduced the logger module, so I change the module name to loggy.

% given a list of nodes that will send it messages
start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(vect:clock(Nodes), []).

loop(Clock, HBQ) ->
    receive 
        {log, From, Time, Msg} ->
            UpdatedClock = vect:update(From, Time, Clock),

        % sort the HBQ above accroding to the time
            UpdatedHBQ = lists:sort(fun(A, B) ->
                {_, TimeA, _} = A,
                {_, TimeB, _} = B,
                lists:foldl(fun({_, T}, Sum) -> T+Sum end, 0, TimeA ) < lists:foldl(fun({_, T}, Sum) -> T+Sum end, 0, TimeB)
            end, [{From, Time, Msg} | HBQ]),
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
            io:format("\nHoldback ~p~nSize of Holdback Queue: ~w~nClock ~p~n", [HBQ, length(HBQ), Clock]),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

% sorting doesn't help
% (1,1,1) 3 true
% (4,2,0) 6 false
% (3,3,3) 9 true

% current: (3,3,3)  9
checkSafe(_, [], UnSafe) ->
    UnSafe;
checkSafe(Clock, [{From, Time, Msg} | Rest], UnSafe) ->
    case vect:safe(Time, Clock) of
        true ->
            log(From, Time, Msg),
            checkSafe(Clock, Rest, UnSafe);
        false ->
            % add the current to HBQ
            checkSafe(Clock, Rest, [{From, Time, Msg} | UnSafe])
    end.
