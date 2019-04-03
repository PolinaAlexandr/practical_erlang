-module(hard_one).
-export([list_users/0]).

list_users() ->
        [
    {user, "Bob", 21, male},
    {user, "Bill", 23, male},
    {user, "Helen", 17, female},
    {user, "Kate", 25, female},
    {user, "John", 20, male}
   ].

group_by_gender_task1(Users) ->
    FutureMap = lists:foldr(
        fun(User, FutureMap) ->
            {user, _, _, Users_Gender} = User,
            Female_Users = maps:get(female, FutureMap),
            Male_Users = maps:get(male, FutureMap),
            if
                Users_Gender = female -> maps:update(female, [User|Female_Users], FutureMap);
                Users_Gender = male -> maps:update(male, [User|Male_Users], FutureMap);
        
            end
        end,
        #{female => [], male => []}, Users),
    FutureMap = Map,
    Map.

