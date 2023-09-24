-module(node1).
-export([start/1, start/2]).
-define(Stabilize, 1000). % 1s
-define(Timeout, 10000). % 10s

% maintain a ring structure; we will be able to add nodes in the ring 
% but not add any elements to the store.

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
    node(Id, Predecessor, Successor).

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
node(Id, Predecessor, Successor) ->
    receive
        {key, Qref, Peer} ->
            % a peer needs to know our key
            Peer ! { Qref, Id},
            node(Id, Predecessor, Successor);
        {notify, New} ->
            % a new node informs us of its existence
            Pred = notify(New, Id, Predecessor),
            node(Id, Pred, Successor);
        {request, Peer} ->
            % a predecessor needs to know our predecessor
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor);
        {status, Pred} ->
            % our successor informs us about its predecessor
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor);
        % the node sends a probe to its successor, and finnally arrives back to the node
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor);
        Msg ->
            io:format("node ~p unexpected received ~p~n", [Id, Msg]),
            node(Id, Predecessor, Successor)
    end.

create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
    Duration = erlang:system_time(micro_seconds) - T,
    io:format("All nodes in the ring: ~p~n", [Nodes]),
    io:format("Duration: ~w micro seconds~n", [Duration]).

% forward the probe to the successor
forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
    Spid ! {probe, Ref, [Id|Nodes], T}.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

% send a request message to its successor. 
stabilize({_, Spid}) ->
    Spid ! {request, self()}.

% Node Id check if its the predecessor node of the successor node is current node itself
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
                    % run stabilize again
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
notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
        nil ->
            {Nkey, Npid};
        {Pkey, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    % the new node is between our predecessor and us
                    {Nkey, Npid};
                false ->
                    Predecessor
            end
    end.
