-module(hist).
-export([new/1, update/3]).


% messages from Name will always be seen as old.
new(Name) ->
    [{Name, -1}].

% check if message number N from the Node is old or new. 
% If it is old, then return old, 
% but if it is new, return {new, Updated} where Updated is the updated history
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {_, OldN} when N =< OldN ->
            old;
        {_, OldN} ->
            NewHistory = lists:keyreplace(Node, 1, History, {Node, N}),
            {new, NewHistory};
        false ->
            {new, [{Node, N} | History]}
    end.