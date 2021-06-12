-module(filter_test).

-include_lib("eunit/include/eunit.hrl").


%--- Setup ---------------------------------------------------------------------


fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun filter_test_1_/0
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


filter_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}],

  Predicate = functions:pred_filter_above_under(3, 5),

  Filter_pid = api:add_filter_proc(Predicate),

  utils:subscribe(self(), Filter_pid),

  Fct =
  fun(X) ->
    send_value(Filter_pid, X, self())
  end,

  lists:map(Fct, Input),


  % Should receive 3 data messages: {3, 0}, {4, 0}, {5, 0}
  receive
    {value, {Val1,_}, _} ->
      ?assertEqual(3, Val1)
  after
    1000 ->
      ?assert(false)
  end,

  receive
    {value, {Val2,_}, _} ->
      ?assertEqual(4, Val2)
  after
    1000 ->
      ?assert(false)
  end,

  receive
    {value, {Val3,_}, _} ->
      ?assertEqual(5, Val3)
  after
    1000 ->
      ?assert(false)
  end,

  receive
    _ ->
      ?assert(false)
  after
    1000 ->
      ?assert(true)
  end.


%---  Aux  ---------------------------------------------------------------------


send_value(Pid, Value, From) ->
  Pid ! {value, Value, From}.