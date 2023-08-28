-module(rudy_multi).
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
            Pid = spawn(fun() -> request(ClientSocket) end),
            spawn(fun() -> 
                Ref = monitor(process, Pid),
                receive 
                    {'DOWN', Ref, process, Pid, normal} ->
                        ok;
                    {'DOWN', Ref, process, Pid, noproc} ->
                        ok;
                    {'DOWN', Ref, process, Pid, Reason} ->
                        io:format("rudy: process ~p died: ~s~n", [Pid, Reason])
                    % show messages received from the process being monitored
                    % Msg ->
                    %     io:format("rudy: process ~p died ~w~n", [Pid, Msg])
                end
            end
                ),
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