-module(http).
-export([parse_request/1, test/0, ok/1, get/1]).

% Get /mysite/index.html HTTP/1.1\r\n
% Host: 10.101.101.10\r\n
% Accept: \*/\*\r\n
% \r\n
parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,
    {{get, URI, Ver}, R3}.

request_uri([32 | R0]) ->
    {[], R0}; % recursive termination
request_uri([C | R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C | Rest], R1}.

http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

% consumes a sequence of headers
headers([13,10 | R0]) ->
    {[], R0}; % recursive termination
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header | Rest], R2}.

% consumes individual header
header([13,10 | R0]) ->
    {[], R0}; % recursive termination
header([C | R0]) ->
    {Rest, R1} = header(R0),
    {[C | Rest], R1}.

message_body(R) ->
    {R, []}.

% Note the double \r\n, 
% one to end the status line and one to end the header section.
ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".

test() ->
    {Req, Headers, Body} = parse_request("GET /index.html HTTP/1.1\r\nfoo 34\r\n\r\nHello"),
    Req = {get, "/index.html", v11},
    Headers = ["foo 34"],
    Body = "Hello".