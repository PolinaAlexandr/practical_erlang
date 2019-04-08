-module(template).

-export([str/0, data/0, parse/2]).

str() ->
    Str = <<"User {{name}} won {{wins}} games and got {{points}} points">>.

data() ->
    Data = #{<<"name">> => "Kate",
             <<"wins">> => 55,
             <<points>> => 777}.


parse(Str, Data) when is_binary(Str) ->
    % maps:find(name, Data),
    Splits1 = binary:split(Str, [<<"{{">>]),
    Splits_2 = lists:maps(fun(Split) ->
        


    Str.
