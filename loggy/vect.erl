-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% example: [{john, 3}, {ringo, 2}]
zero() ->
    [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {_, OldTime} ->
            lists:keyreplace(Name, 1, Time, {Name, OldTime + 1});
        false ->
            [{Name, 1} | Time]
    end.

% the two clocks possibly don’t have same number of entries
merge([], Time) ->
    Time;
merge([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {_, Tj} ->
            [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti} | merge(Rest, Time)]
    end.

% a vector time stamp is less than or equal to another timestamp 
% if each of its entries are less than or equal to the entries of the other timestamp. 
leq([], _) ->
    true;
leq([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            case Ti =< Tj of
                true ->
                    leq(Rest, Time);
                false ->
                    false
            end;
        false ->
            false
    end.


% The clock should reflect what messages we have seen from each of the nodes. 
% The initial clock will thus reflect that we have seen no messages at all.
clock(_Nodes) ->
    [].

update(From, Time, Clock) ->
    % loggy updates its clock when it receives a message from a node
    % so we can find From in Time
    {_, FromTime} = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, {From,FromTime});
        false ->
            [{From, FromTime} | Clock]
    end.

safe(Time, Clock) ->
    leq(Time, Clock).