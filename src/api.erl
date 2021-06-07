-module(api).

-compile([export_all]).

add_sensor_nav() ->
  {ok, Pid} = sensor_nav:start(),
  Pid.


add_sensor_nav(no_init) ->
  {ok, Pid} = sensor_nav:start(1000, no_init),
  Pid.


add_sensor_als() ->
  {ok, Pid} = sensor_als:start(),
  Pid.

add_sensor_als(no_init) ->
  {ok, Pid} = sensor_als:start(1000, no_init),
  Pid.


add_conditional_proc(Predicate) ->
  {ok, Pid} = conditional:start(Predicate),
  Pid.


add_queue_proc(Arg) ->
  {ok, Pid} = myqueue:start(Arg),
  Pid.


add_mean_proc() ->
  {ok, Pid} = aggregate:start(fun functions:mean/1),
  Pid.


add_median_proc() ->
  {ok, Pid} = aggregate:start(fun functions:median/1),
  Pid.


add_max_proc() ->
  {ok, Pid} = aggregate:start(fun functions:max/1),
  Pid.


add_min_proc() ->
  {ok, Pid} = aggregate:start(fun functions:min/1),
  Pid.
