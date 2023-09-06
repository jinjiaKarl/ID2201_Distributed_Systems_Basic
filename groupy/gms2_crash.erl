-module(gms2_crash).
-export([start/1, start/2]).
-define(timout,100).
-define(arghh,100).


start(Id) ->
    Rnd = random:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    io:format("Master ~p started~n", [Id]),
    random:seed(Rnd, Rnd, Rnd),
    leader(Id, Master, [], [Master]),
    ok.

% Id: a unique name of the node, only used for debugging;
% Master: the process identifier of the application layer;
% Slaves: an ordered list of the process identifiers of all slaves in the group;
% Group: a list of all application layer processes in the group.
leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % a message either from its own master(application layer) or from a peer node
            % A message {msg, Msg} is multicasted to all peers, and a message Msg is sent to the application layer.
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} ->
            % a message from a peer or the master that is a request from a node to join the group
            % Wrk: the pro- cess identifier of the application layer
            % Peer: the process identifier of its group process
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            io:format("Group ~p stopped~n", [Id]),
            ok
        end.

bcast(_Id, Msg, Slaves) ->
    % io:format("Group ~p multicasting ~p~n", [_Id, Msg]),
    lists:foreach(fun(Slave) -> Slave ! Msg, crash(_Id) end, Slaves).

crash(Id) ->
    case random:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash~n",[Id]),
            exit(no_luck);
        _ ->
            ok
    end.
        
start(Id, Grp) ->
    Self = self(),
    Rnd = random:uniform(1000),
    {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

% Grp: anyone of application processes in the group
% Master: the leader process identifier of the application layer
init(Id, Rnd, Grp, Master) ->
    io:format("Slave ~p started~n", [Id]),
    random:seed(Rnd, Rnd, Rnd),
    Self = self(),
    % it could be that a node wants to join a dead leader, so we need timeout
    Grp ! {join, Master, Self},
    receive
        {view, [Leader | Slaves], Group} ->
            monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
    after ?timout ->
        Master ! {error, "no reply from leader"}
    end.

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % a request from its master to multicast a message, the message is forwarded to the leader.
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} ->
            % a request from the master to allow a new node to join the group, the message is forwarded to the leader.
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} ->
            % a multicasted message from the leader. A message Msg is sent to the master
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader | Slaves2], Group2} ->
            % a multicasted view from the leader. A view is delivered to the master process.
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, Slaves, Group);
        stop ->
            ok
    end.


election(Id, Master, Slaves, [_|Group]) ->
    io:format("Slave ~p: election~n", [Id]),
    Self = self(),
    case Slaves of
        [Self | Rest] ->
            %  process finds itself being the first node, and it will thus become the leader of the group.
            io:format("Slave ~p: I am the new leader~n", [Id]),
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Slaves, Group);
        [Leader | Rest] ->
            monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
    end.
