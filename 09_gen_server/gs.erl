-module(gs).
-export([start/0, add/3, remove/2, check/2, loop/1, stop/1]).

start() ->
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).

add(Pid, Key, Value) ->
    Pid ! {add, self(),[Key, Value]},
    receive
        {reply, Reply} -> Reply
    after
        5000 -> {error, no_reply}
    end.

remove(Pid, Key) ->
    Pid ! {remove, self(), Key},
    receive
        {reply, Reply} -> Reply
    after
        5000 -> {error, no_reply}
    end.

check(Pid, Key)->
    Pid! {check, self(), Key},
    receive
        {reply, Reply} -> Reply
    after
        5000 -> {error, no_reply}
    end.

stop(Pid) ->
    Pid! stop,
    receive
        {reply, Reply} -> Reply
    after
        5000 -> {error, no_reply}
    end.

loop(State) ->
    io:format("V2, loop ~p~n", [self()]),
    receive
            {add, ClientPid, {K, V}} ->
            State2 = State#{K => V},
            ClientPid ! {reply, ok},
            ?MODULE:loop(State2);
        {remove, ClientPid, K} ->
            State2 = maps:remove(K, State),
            ClientPid ! {reply, ok},
            ?MODULE:loop(State2);
        {check, ClientPid, K} ->
            case maps:find(K, State) of 
                {ok, V} ->  ClientPid !{reply, {ok, V}};
                error -> ClientPid ! {reply, {error, not_found}}
            end,
            ?MODULE:loop(State);
        stop ->
            io:format("~p stops now ~n", [self()]),
            ok; 
            Msg ->
                 io:format("~p got msg ~p~n", [self(), Msg]),
                 ?MODULE:loop(State)
        end.