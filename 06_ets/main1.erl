-module(main1).
-export([list_users/0, init/0, get_young/0, insert_t/0]).
-include_lib("stdlib/include/ms_transform.hrl").

-record(user,{
    name,
    age,
    gender
    }).

list_users() ->
        [
    #user{name ="Bob", age= 21, gender = male},
    #user{name ="Bill", age =23, gender = male},
    #user{ name ="Helen", age =17, gender =female},
    #user{ name ="Kate", age =25, gender =female},
    #user{name = "John", age =20, gender =male}
   ].

init() ->
    Tid = ets:new(table1, [[keypos,2], named_table, set]),
    ets:insert(Tid, list_users()),
    Tid.

insert_t() ->
    MS = ets:run2ms(
           fun(#user{name = Name, age = Age} = User) when Age < 25 -> User end
        ).
    ets:select(table1, MS).


count_records() ->
    
