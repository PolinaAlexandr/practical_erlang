-module(lesson16).
-export([start/0, start/1, start_server/1, start_acceptor/1]).

start() ->
    start(1234).

start(Port) ->
    spawn(?MODULE, start_server, [Port]).

start_server(Port) ->
    io:format("starting ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [list, {active, true}]),
    spawn(?MODULE, start_acceptor, [ListenSocket]),
    timer:sleep(infinity),
    ok.

start_acceptor(ListenSocket) ->
    io:format("Acceptor ~p ~p~n", [self(), ListenSocket]),
    io:format("Waiting ~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Got client ~p ~n", [AcceptSocket]),
    % io:format("ok, take masseges ~p~n", [AcceptSocket]),
    spawn(?MODULE, start_acceptor, [ListenSocket]),
    handle_connect(AcceptSocket).
handle_connect(AcceptSocket) ->
    receive
        {tcp, _, Msg} -> 
            io:format("Hi, ~p ~n", [Msg]),
            gen_tcp:send(AcceptSocket, "Ok ~n", [Msg]),
            handle_connect(AcceptSocket);
        {tcp_closed, _} ->
            io:format("Connection closed ~n"),
            ok
    end.