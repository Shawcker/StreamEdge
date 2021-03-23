%% @doc Module containing available functions used as aggregates or for filtering.

-module(functions).
-export([mean/1, median/1, pred_filter_above/1, pred_filter_under/1, pred_filter_above_under/2]).

%% @doc Gives the mean of items contained the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
%% @spec mean(Values::list({float(), float()})) -> Result::float()
mean(Values) ->
  Fun = fun({Val, _}) -> Val end,
  Only_values_list = lists:map(Fun, Values),
  Sum = lists:sum(Only_values_list),
  Length = length(Only_values_list),
  Sum/Length.


%% @doc Gives the median value of the first 'Amount' items of the list 'Values'.
%% @spec median(Values::list({float(), float()})) -> Result::float()
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


%% PREDICATES

%% @doc Predicate used to filter values above a certain threshold ('Upperbound').
%% @spec pred_filter_above(Upperbound::float()) -> Function::fun((Value::float()) -> boolean())
pred_filter_above(Upperbound) ->
  fun(Value) -> 
    Upperbound >= Value
  end.


%% @doc Predicate used to filter values under a certain threshold ('Lowerbound').
%% @spec pred_filter_under(Lowerbound::float()) -> Function::fun((Value::float()) -> boolean())
pred_filter_under(Lowerbound) ->
  fun(Value) -> 
    Lowerbound =< Value
  end.


%% @doc Predicate used to filter values outside a certain range (outside ['Lowerbound', 'Upperbound']).
%% @spec pred_filter_above_under(Lowerbound::float(), Upperbound::float()) -> Function::fun((Value::float()) -> boolean())
pred_filter_above_under(Lowerbound, Upperbound) ->
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

