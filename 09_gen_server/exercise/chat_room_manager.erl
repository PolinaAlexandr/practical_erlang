-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2, stop/1]).

-record(room, {roomname = [], users = [], messages = []}).

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
