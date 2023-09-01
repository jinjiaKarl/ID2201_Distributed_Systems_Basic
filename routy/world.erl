-module(world).
-export([start/0, get_status/2, check_route/3]).

start() ->
    io:format("connecting stockholm, newyork, beijing ~n"),
    Usa = 'usa@192.168.5.15',
    China = 'china@192.168.5.15',
    Sweden = 'sweden@192.168.5.15',

    {u1, Usa} ! {add, beijing, {c1, China}},
    {c1, China} ! {add, newyork, {u1, Usa}},

    {u1, Usa} ! {add, stockholm, {s1, Sweden}},
    {s1, Sweden} ! {add, newyork, {u1, Usa}},
    timer:sleep(100),

    {u1, Usa} ! broadcast,
    {c1, China} ! broadcast,
    {s1, Sweden} ! broadcast,
    timer:sleep(100),

    {u1, Usa} ! update,
    {u2, Usa} ! update,
    {c1, China} ! update,
    {c2, China} ! update,
    {c3, China} ! update,
    {s1, Sweden} ! update,
    {s2, Sweden} ! update,
    ok.

get_status(Reg, Country) ->
    Usa = 'usa@192.168.5.15',
    China = 'china@192.168.5.15',
    Sweden = 'sweden@192.168.5.15',

    Pid = spawn(fun() ->
            receive
                {status, Msg} ->
                    io:format("Receive status: ~p~n", [Msg])
                end
            end),

    case Country of
        usa ->
            {Reg, Usa} ! {status, Pid};
        china ->
            {Reg, China} ! {status, Pid};
        sweden ->
            {Reg, Sweden} ! {status, Pid};
        _ ->
            io:format("Country not found~n")
    end.

% world:check_route(losangeles, shanghai, china).
% it will show `losangeles: received message (Hello) at final destination`on the usa terminal
check_route(To, From, Country) ->
    Usa = 'usa@192.168.5.15',
    China = 'china@192.168.5.15',
    Sweden = 'sweden@192.168.5.15',
    Msg = "Hello",

    case Country of
        usa ->
            {u1, Usa} ! {route, To, From, Msg};
        china ->
            io:format("china~n"),
            {c1, China} ! {route, To, From, Msg};
        sweden ->
            {s1, Sweden} ! {route, To, From, Msg};
        _ ->
            io:format("Country not found~n")
    end.