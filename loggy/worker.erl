-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Logger, Seed, Sleep, Jitter) ->
    rand:seed(exsss, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Logger, Peers, Sleep, Jitter);
        stop ->
            ok
    end.

% wait for a message from one of its peers or, 
% after a random sleep time, select a peer process that is sent a message.
loop(Name, Logger, Peers, Sleep, Jitter) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            Logger ! {log, Name, Time, {received, Msg}},
            loop(Name, Logger, Peers, Sleep, Jitter);
        stop ->
            ok;
        Error ->
            Logger ! {log, Name, time, {error, Error}}
    after Wait ->
        Selected = select(Peers),
        Time = na,
        Msg = {hello, rand:uniform(100)},
        Selected ! {msg, Time, Msg},
        % If we don’t introduce a delay here, 
        % we would hardly ever have messages occur out of order when running in the same virtual machine.
        jitter(Jitter),
        Logger ! {log, Name, Time, {sending, Msg}},
        loop(Name, Logger, Peers, Sleep, Jitter)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) ->
    ok;
jitter(Jitter) ->
    timer:sleep(rand:uniform(Jitter)). % milliseconds

% inform who their peers are.
peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.