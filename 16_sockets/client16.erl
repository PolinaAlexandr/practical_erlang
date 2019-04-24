-module(client16).
-behavior(gen_server).

-export([start/0, start/1, stop/1]).
-export([init/1, handle_call/3, send/2, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    socket :: gen_tcp:socket()
}).

start()->
    start(1234,2).


start(Port) ->
    Options = [{port, Port}]
    gen_server:start(?MODULE, Options, []).

stop(ClientPid) ->
    gen_server:call(ClientPid, stop).

send(ClientPid, Msg) ->
    gen_server:call(ClientPid, [msg, Msg]).



%%% gen_server API

init(Options) ->
    io:format("~p options:~p~n", [self(), Options]),
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, raw}]),
    io:format("connected: ~p~n", [Socket]),
    State = #state{
              socket = socket
              },
    {ok, State}.

handle_call({msg, Msg}, _From, #state{socket = Socket} = State) ->
    get_tcp:send(Socket, Msg),
    {reply, ok, State}.

handle_call(stop, _From, #state{socket = Socket} =State) ->
    gen_tcp:close(Socket),
    {stop, normal, ok, State},
handle_call(_Request, #state{} = State) ->
    {reply, State}.
handle_call(Request, State) ->
    io:format("cl ~p, ~p~n", [self(), Request]),


handle_info(Request, State) ->
    io:format("client ~p",[self(), Request]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.