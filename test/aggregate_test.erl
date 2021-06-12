-module(aggregate_test).

-include_lib("eunit/include/eunit.hrl").


%--- Setup ---------------------------------------------------------------------


fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun aggregate_test_1_/0
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


aggregate_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}],

  Aggregate_pid = api:add_mean_proc(),
  utils:subscribe(self(), Aggregate_pid),

  send_list(Aggregate_pid, Input, self()),

  % Should receive 1 mean, equal to 3.0
  receive
    {value, {Mean,_}, _} ->
      ?assertEqual(3.0, Mean)
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


send_list(Pid, List, From) ->
  Pid ! {list, List, From}.