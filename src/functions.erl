%% @doc Module containing available functions used as aggregates or for filtering.

-module(functions).
-export([mean/1, median/1, filter_above/3, filter_under/3, filter_above_under/4]).

%% @doc Gives the mean of items contained the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
%% @spec mean(Values::list({float(), float()})) -> {Result::float(), Number_of_values::integer()}
mean(Values) ->
  Fun = fun({Val, _}) -> Val end,
  Only_values_list = lists:map(Fun, Values),
  Sum = lists:sum(Only_values_list),
  Length = length(Only_values_list),
  Sum/Length.


%% @doc Gives the median value of the first 'Amount' items of the list 'Values'.
%% @spec median(Values::list({float(), float()})) -> {Result::float(), Number_of_values::integer()}
median(Values) ->
  Sorted_list = order(Values),
  Length = length(Sorted_list),
  if
    Length rem 2 == 0 ->
      N = ceil(Length/2),
      {{N1,_}, {N2,_}} = {lists:nth(N, Sorted_list), lists:nth(N+1, Sorted_list)},
      (N1+N2)/2;
    true ->
      N = ceil(Length/2),
      {Median,_} = lists:nth(N, Sorted_list),
      Median
  end.


%% @doc Gives the maximum value of the first 'Amount' items of the list 'Values'.
%% @spec max(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
max(Values, Amount) ->
  {Split_list, Length} = split(Values, Amount),
  {lists:last(Split_list), Length}.


%% @doc Gives the maximum value of the first 'Amount' items of the list 'Values'.
%% @spec min(Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}
min(Values, Amount) ->
  {Split_list, Length} = split(Values, Amount),
  [Min|_] = Split_list,
  {Min, Length}.


%% @doc Filters the first 'Amount' items of the list 'Values', by removing any strictly superior to 'Upperbound'.
%% @spec filter_above(Values::list(float()), Amount::integer(), Upperbound::float()) -> {Result::float(), Number_of_values::integer()}
filter_above(Values, Amount, Upperbound) ->
  Predicate = pred_above(Upperbound),
  {Split_list, Length} = split(Values, Amount),
  {lists:filter(Predicate, Split_list), Length}.


%% @doc Filters the first 'Amount' items of the list 'Values', by removing any strictly inferior to 'Lowerbound'.
%% @spec filter_under(Values::list(float()), Amount::integer(), Lowerbound::float()) -> {Result::float(), Number_of_values::integer()}
filter_under(Values, Amount, Lowerbound) ->
  Predicate = pred_under(Lowerbound),
  {Split_list, Length} = split(Values, Amount),
  {lists:filter(Predicate, Split_list), Length}.


%% @doc Filters the first 'Amount' items of the list 'Values', by removing any strictly superior to 'Upperbound' and strictly inferior to 'Lowerbound'.
%% @spec filter_above_under(Values::list(float()), Amount::integer(), Upperbound::float(), Lowerbound::float()) -> {Result::float(), Number_of_values::integer()}
filter_above_under(Values, Amount, Upperbound, Lowerbound) ->
  Predicate = pred_above_under(Upperbound, Lowerbound),
  {Split_list, Length} = split(Values, Amount),
  {lists:filter(Predicate, Split_list), Length}.


%% PREDICATES

pred_above(Upperbound) ->
  fun(Value) -> 
    Upperbound >= Value
  end.


pred_under(Lowerbound) ->
  fun(Value) -> 
    Lowerbound =< Value
  end.


pred_above_under(Upperbound, Lowerbound) ->
  fun(Value) -> 
    Upper = Upperbound >= Value,
    Lower = Lowerbound =< Value,
    Upper and Lower
  end.


%% AUXILIARY FUNCTIONS

% Orders List of items of the form {Value, Timestamp}
order(List) ->
  Compare = fun({A,_}, {B,_}) -> A =< B end,
  lists:sort(Compare, List).


% Takes first Amount items of List and orders them
order_split(List, Amount) ->
  {Split_list, Length} = split(List, Amount),
  {lists:sort(Split_list), Length}.


% Takes first Amount items of List
split(List, Amount) ->
  List_length = length(List),
  if
    List_length =< Amount ->
      {List, List_length};
    true ->
      {Split_list, _} = lists:split(Amount, List),
      {Split_list, Amount}
  end.