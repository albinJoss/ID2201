-module(time).
-compile(export_all).

zero() ->
    0.

inc(Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    lists:max([Ti, Tj]).

leq(Ti, Tj) ->
    if
        Ti =< Tj ->
            true;
        true ->
            false
    end.

clock(Nodes) ->
    lists:foldl(fun(Node, Acc) -> [{Node, zero()} | Acc] end, [], Nodes).

update(Node, Time, Clock) ->
    Updated = lists:keyreplace(Node, 1, Clock, {Node, Time}),
    lists:keysort(2, Updated).

safe(Time, [{_, X} | _]) ->
    leq(Time, X).

