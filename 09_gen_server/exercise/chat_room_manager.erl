-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2, stop/1, loop/1]).

-record(room, {room_name = [], users = [], messages = []}).

start() ->
    Rooms = {},
    spawn(?MODULE, loop, [Rooms]).

% Pid - Server, RoomName - Item
create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}). 


remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


get_rooms(Server) ->
    call(Server, {get_rooms}).


add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, RoomId}).


get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).


stop(Server) ->
        Server ! stop.

call(Servers_Pid, Admn) ->
    Ref = make_ref(),
    Servers_Pid = {self(), Ref, Admn},
    receive
        {reply, Reply, Ref} -> erlang:demomitor(Ref, [flush]),
        Reply
    after
        5000 -> erlang:demonitor(Ref, [flush]),
        {error, noreply}
    end.

loop(Rooms) ->
    io:fwrite("~p enters loop ~n", [self(), map_size(Rooms)]),
    receive
        {Pid, Ref, Admn} -> 
            {Reply, New_Room} = handle_call(Admn, Rooms),
            Pid ! {reply, Ref, Reply}, 
            ?MODULE:loop(New_Room);
        stop -> 
            io:fwrite("process ~p stops now ~n");
        Msg -> io:fwrite("New message ~p", [self(), Msg], ?MODULE:loop(Rooms))
end.


handle_call({create_room, RoomName}, Rooms) ->
    if 
        map_size(Rooms) >5 -> {{error, out_of_limit}, Rooms};
        map_size(Rooms) =< 5 -> RoomId = rand:uniform(100),
        New_Room = Rooms#{RoomId => #room{room_name = RoomName}},
        {{ok, RoomId}, New_Room}
    end;

handle_call({remove_room, RoomId}, Rooms) ->
    case maps:find(RoomId, Rooms) ->
        {ok, _} -> New_Room = maps:remove(RoomId, Rooms), {ok, New_Room};
        error -> {error, not_existing_room}
    end;

handle_call({get_rooms}, Rooms) ->
        A_Maps = maps:fold(fun(Key, #room{roomname = Value}, Acc)-> maps:put(Key, Value, Acc) end, maps:new(), Rooms),
        {maps:to_list(A_Maps), Rooms};
handle_call({add_user, RoomId, UserName}, Rooms)->
    case maps:find(RoomId, Rooms) of
        error -> {{error, room_not_found},Rooms};
        {ok, Values} -> Users = Values#room.users,
    case lists:member(UserName, Users) of
        true ->  {{error, user_is_in_room}, Rooms};
        false -> Rooms2 = Rooms#{RoomId => Values#room{ users = [UserName | Users]}},
                 {ok, Rooms2}
    end
end;

