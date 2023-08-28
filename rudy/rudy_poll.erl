-module(rudy_poll).
-export([init/1]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
        {ok, ListenSocket} ->
            handlers(ListenSocket,10),
            process_flag(trap_exit, true),
            receive
                {'EXIT', From, Reason} ->
                    io:format("rudy: ~p died: ~p~n", [From, Reason]),
                    ok
            end,
            gen_tcp:close(ListenSocket),
            ok;
        {error, _Error} ->
            error
    end.

% Erlang even allows several processes to listen to a socket.
handlers(Listen, N) ->
    case N of
        0 ->
            ok;
        N ->
            spawn(fun() -> handler_poll(Listen) end),
            handlers(Listen, N-1)
    end.

handler_poll(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, ClientSocket} ->
            request(ClientSocket),
            handler_poll(Listen);
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