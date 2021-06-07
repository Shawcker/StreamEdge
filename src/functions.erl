%% @doc Module containing available functions used as aggregates, filtering or any predifined function.

-module(functions).
-export([max/1, min/1, mean/1, median/1, send_trigger/2, pred_filter_above/1, pred_filter_under/1, pred_filter_above_under/2]).

%%% AGGREGATES


%% @doc Gives the maximum of items contained the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
%% @spec max(Values::list({float(), float()})) -> Result::float()
max(Values) ->
  Only_values_list = only_values(Values),
  lists:max(Only_values_list).


%% @doc Gives the minimum of items contained the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
%% @spec min(Values::list({float(), float()})) -> Result::float()
min(Values) ->
  Only_values_list = only_values(Values),
  lists:min(Only_values_list).


%% @doc Gives the mean of items contained the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
%% @spec mean(Values::list({float(), float()})) -> Result::float()
mean(Values) ->
  Only_values_list = only_values(Values),
  Item = lists:nth(1, Only_values_list),
  case Item of
    [_|_] ->
      mean_aux(Only_values_list);
    _ ->
      Sum = lists:sum(Only_values_list),
      Length = length(Only_values_list),
      Sum/Length
  end.

mean_aux([]) ->
  0;
mean_aux(Only_values_list) ->
  mean_aux(Only_values_list, [0,0,0], 0).
mean_aux([], AccList, Acc) ->
  Div = division(Acc),
  lists:map(Div, AccList);
mean_aux([Head|Rest], [Acc1,Acc2,Acc3], Acc) ->
  [A,B,C] = Head,
  mean_aux(Rest, [Acc1+A, Acc2+B, Acc3+C], Acc+1).


division(Divisor) ->
  fun(Elem) ->
    Elem / Divisor
  end.



%% @doc Gives the median value of the first 'Amount' items of the list 'Values'.
%% Items in the list 'Values' are tuples {Value, Timestamp}.
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


%%% TRIGGER

%% @doc Returns a function that sends trigger message to process with pid 'Destination_pid'
%% @spec send_trigger(Sender_pid::integer(), Destination_pid::integer()) -> Fun
send_trigger(Sender_pid, Destination_pid) ->
  fun() ->
    Destination_pid ! {trigger, Sender_pid},
    ok
  end.


%%% PREDICATES

%% @doc Predicate used to filter values above a certain threshold ('Upperbound').
%% @spec pred_filter_above(Upperbound::float()) -> Function::fun((Value) -> boolean())
pred_filter_above(Upperbound) ->
  fun(Value) -> 
    case Value of
      {V1, V2, V3} -> B1 = Upperbound >= V1,
                      B2 = Upperbound >= V2,
                      B3 = Upperbound >= V3,
                      B1 and B2 and B3;

      _            -> Upperbound >= Value
    end
  end.


%% @doc Predicate used to filter values under a certain threshold ('Lowerbound').
%% @spec pred_filter_under(Lowerbound::float()) -> Function::fun((Value) -> boolean())
pred_filter_under(Lowerbound) ->
  fun(Value) ->
    case Value of
      {V1, V2, V3} -> B1 = Lowerbound =< V1,
                      B2 = Lowerbound =< V2,
                      B3 = Lowerbound =< V3,
                      B1 and B2 and B3;

      _            -> Lowerbound =< Value
    end
  end.


%% @doc Predicate used to filter values outside a certain range (outside ['Lowerbound', 'Upperbound']).
%% @spec pred_filter_above_under(Lowerbound::float(), Upperbound::float()) -> Function::fun((Value) -> boolean())
pred_filter_above_under(Lowerbound, Upperbound) ->
  fun(Value) ->
    case Value of
      {V1, V2, V3} -> B1_U = Upperbound >= V1,
                      B1_L = Lowerbound =< V1,
                      B2_U = Upperbound >= V2,
                      B2_L = Lowerbound =< V2,
                      B3_U = Upperbound >= V3,
                      B3_L = Lowerbound =< V3,
                      B1_U and B1_L and B2_U and B2_L and B3_U and B3_L;

      _            -> Upper = Upperbound >= Value,
                      Lower = Lowerbound =< Value,
                      Upper and Lower
    end
  end.


%%% AUXILIARY FUNCTIONS

% Orders List of items of the form {Value, Timestamp}
order(List) ->
  Compare = fun({A,_}, {B,_}) -> A =< B end,
  lists:sort(Compare, List).


% Takes of items {Value, Timestamp} and returns list of Value
only_values(List) ->
  Extract = fun({Value, _Timestamp}) -> Value end,
  lists:map(Extract, List).