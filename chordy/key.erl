-module(key).
-export([generate/0, between/3]).

% a random number from [1, 1.000.000.000 (30 bits)]
generate() ->
    rand:uniform(1000000000).

% check if a Key is between From and To or equal to To; (from, To]
between(Key, From, To) ->
    if
        % key is between From and To
        (From < To) and (Key > From) and (Key =< To) -> true;
        % we’re dealing with a ring, so it could be that From is larger than To 
        (From > To) and ((Key > From) or (Key =< To)) -> true;
        % full circle
        (From =:= To) -> true;
        true -> false
    end.
