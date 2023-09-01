-module(hist).
-export([new/1, update/3]).

% record the history of link-state messages per router.
% tag each constructed message with a per router increasing message number. 

% messages from Name will always be seen as old.
new(Name) ->
    [{Name, -1}].

% check if message number N from the Node is old or new. 
% If it is old, then return old, 
% but if it is new, return {new, Updated} where Updated is the updated history
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {_, OldN} ->
            if N > OldN ->
                {new, lists:keyreplace(Node, 1, History, {Node, N})};
            true ->
                old
            end;
        false ->
            {new, [{Node, N} | History]}
    end.