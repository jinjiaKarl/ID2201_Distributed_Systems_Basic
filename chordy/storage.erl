-module(storage).
-export([create/0, add/3, lookup/2, split/3, merge/2]).


% create a new store
create() ->
    [].

% add a key-value pair, return the updated store
add(Key, Value, Store) ->
    [{Key, Value}|Store].

% return a tuple {Key, Value} or the atom false
lookup(Key, Store) ->
    case lists:keyfind(Key, 1, Store) of
        {Key, Value} -> {Key, Value};
        false -> false
    end.

% return a tuple {Updated, Rest} where the updated store 
% only contains the key-value pairs requested and the rest are found in a list of key-value pairs;
% [(From, To] | Rest] = Store
split(From, To, Store) ->
    lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

% add a list of key-value pairs to a store
merge(Entries, Store) ->
    lists:append(Entries, Store). % Entries ++ Store
