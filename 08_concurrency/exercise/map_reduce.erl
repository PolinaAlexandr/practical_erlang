-module(map_reduce).

-export([start/0, start/1, reduser/1, loop/2, worker/3, parse_file/1, aggrigate/2]).
start()->
    start(["data1.txt", "tada2.txt", "data3.txt", "data4.txt", "data5.txt"]).


start(Files) ->
    spawn(?MODULE, reducer, [Files]),
    receive 
        Res -> {ok, Res}
    after
        3000 -> {error, no_reply}
    end.

reduser(Files) ->
    io:format("reduser ~p~n", [self()]),
    Wait = lists:map(
                fun(File) ->
                    WRef = make_ref(),
                    WPid = spawn(?MODULE, worker, [self(), WRef, File]),
                    {WRef, WPid}
                end,
                Files),
    io:format("wait ~p~n", [Wait]),
    ok.

loop([], Acc) -> Acc;
loop([{Ref, _} | Wait], Acc) ->
    receive
        {Ref, Res} -> loop(Wait, aggrigate(Res, Acc))
    after
        5000 ->
            io:format("ERROR: no reply from worker, ~p~p~n", [Ref, Wait])
    end.

worker(File, Ref, ReduserPid) ->
    io:format("Worker ~p~n", [self()]),
    Res = parse_file(File),
    ReduserPid ! {Ref, Res}.




parse_file(File) ->
    {ok, Content} = file:read_file(File),
    Words = binary:split(Content, [<<"\n">>, <<"\r">>], [global]),
    Words2 = lists:foldr(fun(Word) -> Word/= <<>> end, Words),
    Words3 = lists:map(fun unicode:charackters_to_list/1, Words2),
    lists:foldl(
        fun(Word, Acc) ->
            Acc#{Word => maps:get(Word, Acc, 0) + 1}
        end,
        #{},
        Words3).

aggrigate(Map1, Map2) ->
    maps:fold(
        fun(K, V, Acc) ->
        case maps:find(K, Acc) of
            {ok, VA} -> Acc#{K := VA + V};
            error -> Acc#{K => V}
        end
    end,
    Map1,
    Map2).
            