-module(interfaces).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) ->
        {_, _, Pid} ->
            {ok, Pid};
        false ->
            notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) ->
        {_, Ref, _} ->
            {ok, Ref};
        false ->
            notfound
    end.

name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) ->
        {Name, _, _} ->
            {ok, Name};
        false ->
            notfound
    end.

% return a list with all names.
list([]) ->
    [];
list([{Name, _, _} | Rest]) ->
    [Name | list(Rest)].

% send the message to all interface processes.
broadcast(_, []) ->
    [];
broadcast(Message, [{_, _, Pid} | Rest]) ->
    Pid ! Message,
    broadcast(Message, Rest).