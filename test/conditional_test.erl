-module(conditional_test).

-include_lib("eunit/include/eunit.hrl").


%--- Setup ---------------------------------------------------------------------


fun_test_() ->
  {setup, fun setup/0, fun teardown/1, [
    fun cond_test_1_/0
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


cond_test_1_() ->
  Input = [{1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}],

  Predicate = functions:pred_filter_under(5),

  Cond_pid = api:add_conditional_proc(Predicate),

  Trigger_fct = functions:send_trigger(Cond_pid, self()),

  Cond_pid ! {modify, function, Trigger_fct},

  utils:subscribe(self(), Cond_pid),

  Fct =
  fun(X) ->
    send_value(Cond_pid, X, self())
  end,

  lists:map(Fct, Input),


  % Should receive 1 trigger message, when Conditional gets {5, 0} value
  % Trigger message coming from Cond_pid
  receive
    {trigger, From} ->
      ?assertEqual(Cond_pid, From)
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