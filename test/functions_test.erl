-module(functions_test).

-include_lib("eunit/include/eunit.hrl").

%--- Setup ---------------------------------------------------------------------

fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun mean_test_1_/0,
    fun mean_test_2_/0,
    fun mean_test_error_/0,
    fun median_test_1_/0,
    fun median_test_2_/0,
    fun median_test_3_/0,
    fun mean_test_error_/0,
    fun predicate_above_test_1_/0,
    fun predicate_above_test_2_/0,
    fun predicate_under_test_1_/0,
    fun predicate_under_test_2_/0,
    fun predicate_above_under_test_1_/0,
    fun predicate_above_under_test_2_/0,
    fun predicate_above_under_test_3_/0,
    fun predicate_above_under_test_4_/0
  ]}.

setup() ->
  error_logger:tty(false),
  application:set_env(grisp, drivers, [
    {spi, grisp_emulation_spi_drv},
    {gpio, grisp_emulation_gpio_drv},
    {i2c, grisp_emulation_i2c_drv}
  ]),
  application:set_env(grisp, devices, [
    {spi1, pmod_nav}
  ]),
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


mean_test_error_() ->
  Input = [-4, 7, 1, 10, -50, 156],
  ?assertError(function_clause,
    functions:mean(Input)
  ).


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


median_test_error_() ->
  Input = [-4, 7, 1, 10, -50, 156],
  ?assertError(function_clause,
    functions:median(Input)
  ).


predicate_above_test_1_() ->
  Pred = functions:pred_filter_above(10),
  ?assert(Pred(10)),
  ?assert(not Pred(11)),
  ?assert(Pred(0)),
  ?assert(Pred(-50)),
  ?assert(Pred(0.6)),
  ?assert(Pred(-15.7)).


predicate_above_test_2_() ->
  Pred = functions:pred_filter_above(-5),
  ?assert(Pred(-5)),
  ?assert(Pred(-11)),
  ?assert(not Pred(0)),
  ?assert(Pred(-50)),
  ?assert(not Pred(0.6)),
  ?assert(Pred(-15.7)).


predicate_under_test_1_() ->
  Pred = functions:pred_filter_under(10),
  ?assert(Pred(10)),
  ?assert(not Pred(9)),
  ?assert(not Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(not Pred(0.6)),
  ?assert(Pred(15.7)).


predicate_under_test_2_() ->
  Pred = functions:pred_filter_under(-5),
  ?assert(Pred(-5)),
  ?assert(Pred(9)),
  ?assert(Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(Pred(0.6)),
  ?assert(not Pred(-15.7)).


predicate_above_under_test_1_() ->
  Pred = functions:pred_filter_above_under(10, 5),
  ?assert(not Pred(10)),
  ?assert(not Pred(5)),
  ?assert(not Pred(9)),
  ?assert(not Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(not Pred(0.6)),
  ?assert(not Pred(-15.7)).


predicate_above_under_test_2_() ->
  Pred = functions:pred_filter_above_under(5, 10),
  ?assert(Pred(10)),
  ?assert(Pred(5)),
  ?assert(Pred(7.6)),
  ?assert(Pred(9)),
  ?assert(not Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(not Pred(0.6)),
  ?assert(not Pred(-15.7)).


predicate_above_under_test_3_() ->
  Pred = functions:pred_filter_above_under(-5, 10),
  ?assert(Pred(10)),
  ?assert(Pred(-5)),
  ?assert(Pred(7.6)),
  ?assert(Pred(9)),
  ?assert(Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(Pred(0.6)),
  ?assert(not Pred(-15.7)).


predicate_above_under_test_4_() ->
  Pred = functions:pred_filter_above_under(5, 5),
  ?assert(Pred(5)),
  ?assert(not Pred(7.6)),
  ?assert(not Pred(9)),
  ?assert(not Pred(0)),
  ?assert(not Pred(-50)),
  ?assert(not Pred(0.6)),
  ?assert(not Pred(-15.7)).