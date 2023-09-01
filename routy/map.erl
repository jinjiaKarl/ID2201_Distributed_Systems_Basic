-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% each entry consists of a city with a list of directly connected cities.
new() ->
    [].

update(Node, Links, Map) ->
    % if Node is not in Map, it will return original Map
    NewMap = lists:keydelete(Node, 1, Map),
    [{Node, Links} | NewMap].

reachable(Node, Map) ->
    Result = lists:keyfind(Node, 1, Map),
    case Result of
        false -> [];
        {_, Links} -> Links
    end.

% [{berlin,[london,paris]}]
% all_nodes([]) ->
%     [];
% all_nodes([ { Node, Links }| Rest]) ->
%     List = [Node, Links | all_nodes(Rest)],
%     NewList = lists:usort(lists:flatten(List)),
%     lists:reverse(NewList).
% second version using foldl
all_nodes(Map) ->
    NewList = lists:foldl(fun( { Node, Links }, Acc) ->
        lists:usort(lists:flatten([Node, Links, Acc]))
    end, [], Map),
    lists:reverse(NewList).




