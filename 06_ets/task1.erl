-module(task1).
-export([init/0, fill_data/1, count_records/1]).

init() ->
    ets:new(t_data, [set, named_table]),
    fill_data(20).

fill_data(Max_Elements) ->
    fill_data(1, Max_Elements).


fill_data(Current_ID, Max_Elements) when Current_ID > Max_Elements -> ok;
fill_data(Current_ID, Max_Elements) ->
    Source = get_random_source(),
    Num = rand:uniform(100500),
    ets:insert(t_data, {Current_ID, Source, Num}),
    fill_data(Current_ID + 1, Max_Elements).


get_random_source() ->
    source = [source1, source2, source3, source4, source5],
    lists:nth(rand:uniform(5), source).

count_records(Sources) ->
    ets:match(t_data, {})

            
        
