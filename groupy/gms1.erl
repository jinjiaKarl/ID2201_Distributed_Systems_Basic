-module(gms1).
-export([start/1, start/2]).


start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    io:format("Master ~p started~n", [Id]),
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

bcast(Id, Msg, Slaves) ->
    io:format("Group ~p multicasting ~p~n", [Id, Msg]),
    lists:foreach(fun(Slave) -> Slave ! Msg end, Slaves).
        
start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

% Grp: anyone of application processes in the group
% Master: the leader process identifier of the application layer
init(Id, Grp, Master) ->
    io:format("Slave ~p started~n", [Id]),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, [Leader | Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group)
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
        stop ->
            ok
    end.