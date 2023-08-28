-module(test).
-export([bench/3]).

% 192.168.105.2:8080

bench(Host, Port, Count) ->
    Start = erlang:system_time(micro_seconds),
    run(Count, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Elapsed = Finish - Start,
    io:format("test: ~w requests in ~w s~n", [Count, Elapsed/1000000]),
    % calucte request per second
    Qps = Count / (Elapsed / 1000000),
    io:format("test: ~w requests per second~n", [Qps]).

run(N, Host, Port) ->
    if
        N == 0 ->
            ok;
        true ->
            request(Host, Port),
            run(N-1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("/foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
        {ok, _} ->
            ok;
        {error, Error} ->
            io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
