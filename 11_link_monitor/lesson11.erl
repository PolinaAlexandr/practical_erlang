-module(lesson11).
-export([run/0, worker/1]).

run()->
    lists:foldl(
            fun(ID) -> spawn_link(fun() -> worker(ID) end) end,
            lists:seq(1, 10)
            ),
        ok.

worker(ID) ->
    io:format("worker ID:~p, stap~n", [ID, self()]),
    if 
        ID rem 5 == 0 -> throw(worker, )
    timer:sleep(rand:form(1000)),
    io:format("worker ID:~p, stap~n", [ID, self()]),
    ok.