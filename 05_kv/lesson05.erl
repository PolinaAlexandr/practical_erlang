-module(lesson05).
-export([users_to_upper/1, remove_young_users/1, get_names/1]).

users_to_upper(Users) when is_map(Users) ->
    maps:map(
        fun
            (ID, User) when ID rem 2 == 0 ->
                {user, Name, Age, Gender} = User,
                {user, string:users_to_upper(Name), Age, Gender};
                (_ID, User ) -> User
        end,
        Users).


remove_young_users(Users) ->
    maps:filter(
        fun(_ID, User) ->
            {user, _, Age, _} = User,
            Age = 25
        end,
        Users).


get_names(Users) ->
    maps:fold(
        fun(_ID, User, Acc) ->
            {User, Name, _, _} = User,
            {Name, Acc}
        end, 
        [],
        Users).
