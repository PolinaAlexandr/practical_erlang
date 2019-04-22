-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {
    rooms = []
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

create_room(RoomName) ->
    {ok, RoomsPid} = chat_room:start_link(),
    gen_server:cast(?MODULE, {create_room, {RoomName, RoomsPid}}),
    {RoomName, RoomsPid}.

get_rooms() ->
    gen_server:call(?MODULE, get_rooms).


add_user(RoomsPid, UserName, PidUser) ->
    gen_server:call(?MODULE, {add_user, {RoomsPid, UserName, PidUser}}).


remove_user(RoomsPid, PidUser) ->
    gen_server:call(?MODULE, {remove_user, {RoomsPid, PidUser}}).


get_users(RoomsPid) ->
    gen_server:call(?MODULE, {get_users, RoomsPid}).


send_message(RoomsPid, UserName, Message) ->
    gen_server:call(?MODULE, {send_message, {RoomsPid, UserName, Message}}).


get_history(RoomsPid) ->
    gen_server:call(?MODULE, {get_history, RoomsPid}).

init([]) -> 
    {ok, #state{}}.


handle_cast({create_room, {RoomName, RoomsPid}}, #state{rooms = Rooms} = State) ->
    NewState = State#state{rooms = [{RoomName, RoomsPid} | Rooms]},
    {noreply, NewState}.


handle_call({close_room, RoomsPid}, _From, #state{rooms = Rooms} = State) ->
    {Reply, State1} = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {{error, room_not_found}, State};
                true -> NewState = State#state{rooms = lists:keydelete(RoomsPid, 2, Rooms)},
                {chat_room:stop(RoomsPid), NewState}
            end,
    {reply, Reply, State1};


handle_call(get_rooms, _From, State) ->
    {reply, State#state.rooms, State};


handle_call({add_user, {RoomsPid, UserName, PidUser}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:add_user(RoomsPid, UserName, PidUser), ok
            end,
    {reply, Reply, State};


handle_call({remove_user, {RoomsPid, PidUser}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:remove_user(RoomsPid, PidUser)
            end,
    {reply, Reply, State};


handle_call({get_users, RoomsPid}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> {ok, chat_room:get_users(RoomsPid)}
            end,
    {reply, Reply, State};


handle_call({send_message, {RoomsPid, UserName, Message}}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> chat_room:add_message(RoomsPid, UserName, Message)
            end,
    {reply, Reply, State};


handle_call({get_history, RoomsPid}, _From, #state{rooms = Rooms} = State) ->
    Reply = case lists:keymember(RoomsPid, 2, Rooms) of 
                false -> {error, room_not_found};
                true -> {ok, chat_room:get_history(RoomsPid)}
            end,
    {reply, Reply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) -> 
    {ok, State}.
