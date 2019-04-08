-module(spawn).
-export([run_processes/1, run_process/1, mailbox_checker/0, loop/0, test_mailbox/0]).

run_process(ID) ->
    timer:sleep(500),
    io:format("I am ~p with ID: ~p~n", [self(), ID]),
    ok.

run_processes(Num) ->
    lists:foreach(
        fun(ID) -> spawn(?MODULE, run_process, [ID]) end,
        lists:seq(1, Num)
).

mailbox_checker() ->
    io:format("I am mailbox checker, send massege~p~n", [self()]),
    loop().

loop()->    
    receive
        stop ->
            io:format("mailbox checker ~p stops now~n ", [self()]),
            ok;
        Msg -> 
            io:format("mailbox checker ~p got ~p~n", [self(), Msg]),
            loop()
    after
        10000 ->
             io:format("mailbox checker ~p, no messages ~n", [self()]),
             loop()
    end.

test_mailbox() ->
    spawn(?MODULE, mailbox_checker, []).