-module(lesson16).
-export([start/0, start/2, start_server/2, start_acceptor/2]).

start() ->
    start(1234, 2).

start(Port, NumAcceptor) ->
    spawn(?MODULE, start_server, [Port, NumAcceptor]).

start_server(Port, NumAcceptor) ->
    io:format("starting ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [list, {active, true}]),
    [spawn(?MODULE, start_acceptor, [ID, ListenSocket]) || ID <- lists:seq(1, NumAcceptor)],
    timer:sleep(infinity),
    ok.

start_acceptor(ListenSocket, ID) ->
    io:format("Acceptor ~p ~p~n", [self(), ID]),
    io:format("Waiting ~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Got client ~p ~n", [AcceptSocket]),
    % io:format("ok, take masseges ~p~n", [AcceptSocket]),
    handle_connect(ID, ListenSocket).

handle_connect(ID, ListenSocket) ->
    receive
        {tcp, AcceptSocket, Msg} -> 
            io:format("~p:~p: ur msg:~p~n", [self(), ID, Msg]),
            gen_tcp:send(AcceptSocket, "Ok ~n" ++ integer_to_list(ID) ++ "." ++ Msg),
            handle_connect(ID, ListenSocket);
        {tcp_closed, _} ->
            io:format("Connection closed ~n"),
            start_acceptor(ID, ListenSocket)
            
    end.