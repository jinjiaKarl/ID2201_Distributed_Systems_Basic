-module(rudy).
-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, ListenSocket} ->
            handler(ListenSocket),
            gen_tcp:close(ListenSocket),
            ok;
        {error, _Error} ->
            error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, ClientSocket} ->
            request(ClientSocket),
            % listen to a new connection once the first has been handled
            handler(Listen);
        {error, _Error} ->
            error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of 
        {ok, Str}  ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~s~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40), % milliseconds
    http:ok(URI).