-module(node2).
-export([start/1, start/2]).
-define(Stabilize, 1000). % 1s
-define(Timeout, 10000). % 10s

% add a local store to each node and the possibility to add and search for key-value pairs.

% we are the first node in the ring
start(Id) ->
    start(Id, nil).

% we’re connecting to an existing ring.
start(Id, Peer) ->
    timer:start(), % start timer server
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store = storage:create(),
    node(Id, Predecessor, Successor, Store).

% we are the first node in the ring
connect(Id, nil) ->
    {ok, {Id, self()}};
% we are connecting to an existing ring
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, Peer}}
    after ?Timeout ->
        io:format("~p connect ~p timeout~n", [Id, Peer])
    end.

%Id: Node identifier, aka key
%Predecessor: {Id, Pid} of predecessor
%Successor: {Id, Pid} of successor
node(Id, Predecessor, Successor, Store) ->
    receive
        {key, Qref, Peer} ->
            % a peer needs to know our key
            Peer ! { Qref, Id},
            node(Id, Predecessor, Successor, Store);
        {notify, New} ->
            % a new node informs us of its existence
            {Pred, NewStore} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, NewStore);
        {request, Peer} ->
            % a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        {status, Pred} ->
            % our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Store);
        % the node sends a probe to its successor, and finnally arrives back to the node
        probe ->
            create_probe(Id, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(Id, T, Nodes, Store),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {add, Key, Value, Qref, Client} ->
            % adding an element
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client} ->
            % lookup an element
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);
        Msg ->
            io:format("node ~p unexpected received ~p~n", [Id, Msg]),
            node(Id, Predecessor, Successor, Store)
    end.

create_probe(Id, {_, Spid}, Store) ->
    io:format("Node ~p starting probe, store: ~p~n",[Id, Store]),
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(Id, T, Nodes, Store) ->
    Duration = erlang:system_time(micro_seconds) - T,
    io:format("Node ~p endning probe, store: ~p~n",[Id, Store]),
    io:format("All nodes in the ring: ~p, duration: ~w micro seceonds~n", [Nodes, Duration]).

% forward the probe to the successor
forward_probe(Ref, T, Nodes, Id, {_, Spid}, Store) ->
    io:format("Node ~p store: ~p~n",[Id, Store]),
    Spid ! {probe, Ref, [Id|Nodes], T}.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% send a request message to its successor. 
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

% Node Id check if its the predecessor node of the successor node itself
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        nil ->
            % if predecessor is empty, inform the successor about our existence
            % the successor has no predecessor, so Node Id is the predecessor
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _} ->
            % Node Id is the predecessor
            Successor;
        {Skey, _} ->
            % the predecessor of the successor is the successor, so Node Id is the predecessor
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid} ->
            % the predecessor of the successor is another node
            case key:between(Xkey, Id, Skey) of
                true ->
                    % the predecessor of the successor is between Node Id and the successor
                    % so the other node is our new successor
                    Xpid ! {request, self()},
                    Pred;
                false ->
                    % Node Id is between the predecessor of the successor and the successor
                    % so Node Id is the new predecessor of the successor
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
    end.

% inform the peer that sent the request about our predecessor
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

% Being notified of a node is a way for a node to
% make a friendly proposal that it might be our proper predecessor
notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
        nil ->
            % give part of a store to the predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % the new node is between our predecessor and us
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

handover(Id, Store, Nkey, Npid) ->
    % (Id, Nkey] = Rest, (Nkey, Id] = Keep
    {Rest, Keep} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest}, % (Id, Nkey] -> Npid (predecessor)
    Keep.

% A node will take care of all keys from (but not including) the identifier of its predecessor 
% to (and including) the identifier of itself.  (Pkey, Id]  -> Id
% If we are not responsible, we send an add message to our successor.
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            % we are responsible
            Client ! {Qref, ok},
            NewStore = storage:add(Key, Value, Store),
            NewStore;
        false ->
            % we are not responsible, sending to successor
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

% determine if we are responsible for the key. 
% If so, we do a simple lookup in the local store and then send the reply to the requester. 
% If it is not our responsibility, we will forward the request.
lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            % we are responsible
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            % we are not responsible, forwarding to successor
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.