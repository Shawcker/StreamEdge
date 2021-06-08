-module(queue_test).

-include_lib("eunit/include/eunit.hrl").


%--- Setup ---------------------------------------------------------------------


fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun queue_test_1_/0,
    fun queue_test_2_/0,
    fun queue_test_3_/0
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


queue_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}],

  Queue_pid = api:add_queue_proc(5),
  utils:subscribe(self(), Queue_pid),

  Fct =
  fun(X) ->
    send_value(Queue_pid, X, self())
  end,

  lists:map(Fct, Input),


  % With 5 values, should receive 1 list of 5 items
  receive
    {list, List, _} ->
      ?assertEqual(5, length(List))
  after
    1000 ->
      ?assert(false)
  end.


queue_test_2_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}],

  Queue_pid = api:add_queue_proc(5),
  utils:subscribe(self(), Queue_pid),

  Fct =
  fun(X) ->
    send_value(Queue_pid, X, self())
  end,

  lists:map(Fct, Input),


  % With 6 values, should receive 2 lists and nothing more
  receive
    {list, _, _} ->
      ok
  after
    1000 ->
      ?assert(false)
  end,

  receive
    {list, _, _} ->
      ok
  after
    1000 ->
      ?assert(false)
  end,

  receive
    _ -> ?assert(false)
  after
    1000 ->
      ?assert(true)
  end.


queue_test_3_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}, {7, 0}],

  Queue_pid = api:add_queue_proc(5),
  utils:subscribe(self(), Queue_pid),

  Fct =
  fun(X) ->
    send_value(Queue_pid, X, self())
  end,

  lists:map(Fct, Input),


  % With 7 values, should receive 3 list of 5 items, sliding
  % List1 = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}]
  % List2 = [{2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}]
  % List3 = [{3, 0}, {4, 0}, {5, 0}, {6, 0}, {7, 0}]
  receive
    {list, List1, _} ->
      ?assertEqual({1, 0}, lists:nth(1, List1)),
      ?assertEqual({2, 0}, lists:nth(2, List1)),
      ?assertEqual({3, 0}, lists:nth(3, List1)),
      ?assertEqual({4, 0}, lists:nth(4, List1)),
      ?assertEqual({5, 0}, lists:nth(5, List1))
  after
    1000 ->
      ?assert(false)
  end,

  receive
    {list, List2, _} ->
      ?assertEqual({2, 0}, lists:nth(1, List2)),
      ?assertEqual({3, 0}, lists:nth(2, List2)),
      ?assertEqual({4, 0}, lists:nth(3, List2)),
      ?assertEqual({5, 0}, lists:nth(4, List2)),
      ?assertEqual({6, 0}, lists:nth(5, List2))
  after
    1000 ->
      ?assert(false)
  end,

  receive
    {list, List3, _} ->
      ?assertEqual({3, 0}, lists:nth(1, List3)),
      ?assertEqual({4, 0}, lists:nth(2, List3)),
      ?assertEqual({5, 0}, lists:nth(3, List3)),
      ?assertEqual({6, 0}, lists:nth(4, List3)),
      ?assertEqual({7, 0}, lists:nth(5, List3))
  after
    1000 ->
      ?assert(false)
  end.


%---  Aux  ---------------------------------------------------------------------


send_value(Pid, Value, From) ->
  Pid ! {value, Value, From}.