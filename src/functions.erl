%% @doc Module containing available aggregate functions.

-module(functions).
-export([mean/2, median/2, max/2, min/2]).

%% @doc Mean function.
%% @spec mean(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
mean(Values, Amount) ->
  mean(Values, Amount, Amount, 0).

mean([], Amount, Remaining, Acc) ->
  {Acc/Amount, Amount-Remaining};
mean(_Values, Amount, 0, Acc) ->
  {Acc/Amount, Amount};
mean([V|Rest], Amount, Remaining, Acc) ->
  mean(Rest, Amount, Remaining-1, Acc+V).

%% @doc Median function.
%% @spec median(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
median(Values, Amount) ->
  {Sorted_split_list, Length} = order_split(Values, Amount),
  io:format("N = ~p~n", [Sorted_split_list]),
  if
    Length rem 2 == 0 ->
      N = Length/2,
      {N1, N2} = {lists:nth(N, Sorted_split_list), lists:nth(N+1, Sorted_split_list)},
      {(N1+N2)/2, Length};
    true ->
      N = ceil(Length/2),
      {lists:nth(N, Sorted_split_list), Length}
  end.


%% @doc Max function.
%% @spec max(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
max(Values, Amount) ->
  List_length = length(Values),
  if
    List_length =< Amount -> Max = lists:max(Values),
                             {Max, List_length};
    true                  -> {Split_list, _} = lists:split(Amount, Values),
                             Max = lists:max(Split_list),
                             {Max, Amount}
  end.


%% @doc Min function.
%% @spec min(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
min(Values, Amount) ->
  List_length = length(Values),
  if
    List_length =< Amount -> Min = lists:min(Values),
                             {Min, List_length};
    true                  -> {Split_list, _} = lists:split(Amount, Values),
                             Min = lists:min(Split_list),
                             {Min, Amount}
  end.


%% AUXILIARY FUNCTIONS

% Takes first @param Amount items of @param List and orders them
order_split(List, Amount) ->
  List_length = length(List),
  if
    List_length =< Amount ->
      {lists:sort(List), List_length};
    true ->
      {Split_list, _} = lists:split(Amount, List),
      {lists:sort(Split_list), Amount}
  end.