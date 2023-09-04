-module(worker_vect).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Logger, Seed, Sleep, Jitter) ->
    rand:seed(exsss, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Logger, Peers, Sleep, Jitter, vect:zero());
        stop ->
            ok
    end.

% wait for a message from one of its peers or, 
% after a random sleep time, select a peer process that is sent a message.
loop(Name, Logger, Peers, Sleep, Jitter, CurTime) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            UpdatedTime = vect:inc(Name, vect:merge(CurTime, Time)),
            Logger ! {log, Name, UpdatedTime, {received, Msg}},
            loop(Name, Logger, Peers, Sleep, Jitter, UpdatedTime);
        stop ->
            ok;
        Error ->
            Logger ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = vect:inc(Name, CurTime),
        Msg = {hello, rand:uniform(100)},
        Selected ! {msg, Time, Msg},
        % If we don’t introduce a delay here, 
        % we would hardly ever have messages occur out of order when running in the same virtual machine.
        jitter(Jitter),
        Logger ! {log, Name, Time, {sending, Msg}},
        loop(Name, Logger, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) ->
    ok;
jitter(Jitter) ->
    timer:sleep(rand:uniform(Jitter)). % milliseconds

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.