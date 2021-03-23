-module(functions_test).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun mean_test_1_/0,
    fun mean_test_2_/0,
    fun median_test_1_/0,
    fun median_test_2_/0,
    fun median_test_3_/0
  ]}.

setup() ->
  error_logger:tty(false),
  {ok, Apps} = application:ensure_all_started(grisp),
  Apps.

teardown(Apps) ->
  [application:stop(A) || A <- lists:reverse(Apps)],
  error_logger:tty(true).

%--- Tests ---------------------------------------------------------------------

mean_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}],
  Mean = functions:mean(Input),
  ?assertEqual(3.5, Mean).


mean_test_2_() ->
  Input = [{-4, 0}, {7, 0}, {1, 0}, {10, 0}, {-50, 0}, {156, 0}],
  Mean = functions:mean(Input),
  ?assertEqual(20.0, Mean).


median_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}],
  Median = functions:median(Input),
  ?assertEqual(3.5, Median).


median_test_2_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}, {7, 0}],
  Median = functions:median(Input),
  ?assertEqual(4, Median).


median_test_3_() ->
  Input = [{-4, 0}, {7, 0}, {1, 0}, {10, 0}, {-50, 0}, {156, 0}],
  Median = functions:median(Input),
  ?assertEqual(4.0, Median).
