-module(learn_list).
-compile(export_all).

test_keyfind() ->
    Key = 1,
    FindResult = lists:keyfind(Key, 1, [{2, 2}, {1,3}]),
    case FindResult of
        false -> io:format("not found~n");
        {_, Value} -> io:format("found value: ~p~n", [Value])
    end.


test_keydelete() ->
    Key = 1,
    TupleList = lists:keydelete(Key, 1, [{1, 2}, {2,3}]),
    io:format("TupleList: ~p~n", [TupleList]).

test_map() ->
    List1 = [1, 2, 3],
    List2 = lists:map(fun(X) -> X * 2 end, List1),
    io:format("List2: ~p~n", [List2]).

% similar to js reduce
test_foldl() ->
    List1 = [1, 2, 3],
    List2 = lists:foldl(fun(X, Sum) -> X + Sum end, 0, List1),
    io:format("List2: ~p~n", [List2]).