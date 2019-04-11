-module(gs).
-export([start/0, add/3, remove/2, check/2, call/3, stop/1, handle_call/3]).

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

call(ServerPid, Command, Payload)->
    QueryRef = erlang:monitor(process, ServerPid),
    ServerPid ! {Command, QueryRef, self(),Payload},
    receive
        {reply,QueryRef, Reply} -> Reply,
            erlang:demonitor(QueryRef, [flush]),
            Reply;
        {'DOWN', QueryRef, process, ServerPid, Reason} ->
            io:format("Server crashed with reason: ~p~n", [Reason]),
            {error, Reason}

    after
        5000 ->
            erlang:demonitor(QueryRef, [flush]),
            {error, no_reply}
    end.

    handle_call(add, {K, V}, State) ->
        State2 = State#{K => V},
        {ok, State2};
    handle_call(remove, K, State) ->
        State2 = maps:remove(K, State),
        {ok, State2};
    handle_call(check, 42, State) ->
        42/0,
        {ok, State};
    handle_call(check, K, State) ->
        Reply = case maps:find(K, State) of
            {ok, V} -> {ok, V};
            error -> {error, not_found}
        end,
    {Reply, State}.
