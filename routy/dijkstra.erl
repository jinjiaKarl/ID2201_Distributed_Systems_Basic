-module(dijkstra).
-export([table/2, route/2]).
% -export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

% a sorted list of entry by distance
% [{berlin, 2, paris}, {london, 3, paris}]
% means berlin could be reached in 2 hops using paris as a gateway 


% returns the length of the shortest path to the node or 0 if the node is not found.
entry(Node, Sorted) ->
    Result = lists:keyfind(Node, 1, Sorted),
    case Result of
        false -> 0;
        {_,N,_} -> N
    end.

% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway.
replace(Node, N, Gateway, Sorted) ->
   lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway})).

% updates the entry if N is smaller than the current length.
update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        OldN when N < OldN ->
            replace(Node, N, Gateway, Sorted);
        _ ->
            Sorted
    end.

% no more entried in the sorted list
iterate([], _, Table) ->
    Table;
% the first entry in the sorted list is a dummy entry with infinite path
iterate([{_, inf, _} | _], _, Table) ->
    Table;
% take the first entry in the sorted list, find the nodes in the map reachable from this entry, 
% and for each of these nodes, update the Sorted list. 
% The entry that you took from the sorted list is added to the routing table.
iterate([{Node, N, Gateway} | Rest], Map, Table) ->
    ReachableCities = map:reachable(Node, Map),
    Updates = lists:foldl(fun(Nd, Sorted) -> update(Nd, N+1, Gateway, Sorted) end, Rest, ReachableCities),
    iterate(Updates, Map, [{Node, Gateway} | Table]).

% return a list of entries where each entry states which gateway to use to find the shortest path to a node
table(Gateways, Map) ->
    Nodes = map:all_nodes(Map),
    InitSortedList = lists:map(fun(X) -> {X, inf, unknown} end, Nodes),
    SortedList = lists:foldl(fun(Node, Sorted) -> update(Node, 0, Node, Sorted) end, InitSortedList, Gateways),
    iterate(SortedList, Map, []).

% search the routing table and return the gateway suitable to route messages to a node.
route(Node, Table)->
	case lists:keyfind(Node, 1, Table) of
		{_, Gateway}->
			{ok, Gateway};
		false->
			notfound
	end.
