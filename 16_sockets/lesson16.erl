-module(lesson16).
-export([start/0, start/2,start_server/2, start_acceptor/2]).

start() ->
    start(1234, 2).


start(Port, NumAcceptor) ->
    spawn(?MODULE, start_server, [Port, NumAcceptor]).

start_server(Port, NumAcceptor) ->
    io:format("starting ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    [spawn(?MODULE, start_acceptor, [ID, ListenSocket]) || ID <- lists:seq(1, NumAcceptor)],
    timer:sleep(infinity),
    ok.

start_acceptor(ListenSocket, ID) ->
    io:format("Acceptor ~p ~p~n", [self(), ID]),
    io:format("Waiting ~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Got client ~p ~n", [AcceptSocket]),
    % io:format("ok, take masseges ~p~n", [AcceptSocket]),
    handle_connect(ID, ListenSocket, AcceptSocket).

handle_connect(ID, ListenSocket, AcceptSocket) ->
    case gen_tcp:recv(AcceptSocket, 2) of
        {ok, Msg} -> 
            {ok, Msg} = gen_tcp:recv(AcceptSocket, 0),
            io:format("~p ~p: help: ~p~n ", [self(), ID, Msg]),
            BinID = list_to_binary(integer_to_list(ID)),
            gen_tcp:send(AcceptSocket, <<"ECHO", BinID/binary, ".", Msg/binary>>),
            handle_connect(ID, ListenSocket, AcceptSocket);
        {error, closed} ->
            io:format("Connection closed:~n"),
            start_acceptor(ID, ListenSocket)
    end.