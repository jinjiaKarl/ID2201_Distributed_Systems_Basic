-module(routy).
-export([start/2, stop/1]).

% erl -name sweden@192.168.5.15 -setcookie routy -connect_all false

% Reg: register name of the process, e.g. r1
% Name: the logical name of the process, e.g. stockholm
start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

stop(Reg) ->
    Reg ! stop,
    unregister(Reg).

init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

% Name: the logical name of the process, e.g. stockholm
% N: the sequence number of the link-state message
% Hist: history of the link-state messages
% Intf: interfaces
% Table: routing table
% Map: directional network map
router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Intf1 = interfaces:add(Node, Ref, Pid, Intf),
            io:format("~w: added ~w~n", [Name, Node]),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} ->
            {ok, Ref} = interfaces:ref(Node, Intf),
            erlang:demoitor(Ref),
            io:format("~w: removed ~w~n", [Name, Node]),
            Intf1 = interfaces:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _} ->
            {ok, Down} = interfaces:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = interfaces:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            io:format("~w: status sent to ~w~n", [Name, From]),
            router(Name, N, Hist, Intf, Table, Map);
        {links, Node, R, Links} ->
            % receive the link-state messages
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    interfaces:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        update ->
            Table1 = dijkstra:table(interfaces:list(Intf), Map),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast ->
            Message = {links, Name, N, interfaces:list(Intf)},
            interfaces:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        {route, Name, From, Message} ->
            % message arrived at the final destination
            io:format("~w: received message ~w ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} ->
            io:format("~w: routing message of (~w)", [Name, Message]),
		    %% Find the gateway we want to route the message to
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case interfaces:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            %% Send the message to the gateway
                            Pid ! {route, To, From, Message};
                        notfound ->
                            ok
                    end;
                notfound ->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message} ->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        stop ->
            ok
        end.

