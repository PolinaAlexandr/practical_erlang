-module(hard_one).
-export([list_users/0, group_by_gender/1, criteriafunc_age/1, criteriafunc_gender/1, group_by/2]).


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
                Users_Gender == female -> maps:update(female, [User|Female_Users], FutureMap);
                Users_Gender == male -> maps:update(male, [User|Male_Users], FutureMap)
        
            end
        end,
        #{female => [], male => []}, Users),
    FutureMap.


criteriafunc_age(User) ->
    {user, _Name, Age, _Users_Gender} = User, 
    if 
        Age > 0, Age =< 12 -> child;
        Age > 12, Age =< 18 -> teeneage;
        Age > 18, Age =< 25 -> young; 
        Age > 25, Age =< 60 -> adult;
        Age > 60 -> old
    end. 

criteriafunc_gender(User) ->
    {user, _Name, _Age, Users_Gender} = User,
    Users_Gender.


group_by(CriteriaFun, Users) ->
        lists:foldl(
            fun(User, Acc) ->
                Category = CriteriaFun(User),
                case maps:find(Category, Acc) of 
                    error -> maps:put(Category, [User], Acc);
                    {ok,Values} -> maps:put(Category,[User | Values], Acc)
                end
            end,
            #{},
            Users).
    



