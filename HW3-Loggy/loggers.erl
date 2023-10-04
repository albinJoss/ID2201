-module(loggers).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
    receive 
        {log, From, Time, Msg} ->
            UpdatedClock = time:update(From, Time, Clock),
            UpdatedQ = [{From, Time, Msg} | Queue],
            SortedQ = lists:keysort(2, UpdatedQ),
            NewQ = safetyCheck(SortedQ, UpdatedClock, []),
            %log(From, Time, Msg),
            loop(UpdatedClock, NewQ);
        stop ->
            io:format("There are a total of ~w messages in the queue.~n", [length(Queue)]),
            ok
    end.

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p ~n", [Time, From, Msg]).

safetyCheck([], _, Acc) ->
    Acc;

safetyCheck([{From, Time, Msg}|T], Clock, Acc) ->
    case time:safe(Time, Clock) of
        true ->
            log(From, Time, Msg),
            safetyCheck(T, Clock, Acc);
        false ->
            safetyCheck(T, Clock, [{From, Time, Msg} | Acc])
    end.
