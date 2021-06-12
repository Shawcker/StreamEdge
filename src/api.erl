-module(api).

-compile([export_all]).


% ===== SENSOR NAV =====
add_sensor_nav() ->
  {ok, Pid} = sensor_nav:start(),
  Pid.

add_sensor_nav(no_init) ->
  {ok, Pid} = sensor_nav:start(1000, no_init),
  Pid;
add_sensor_nav(Rate) ->
  {ok, Pid} = sensor_nav:start(Rate),
  Pid.

add_sensor_nav(Rate, no_init) ->
  {ok, Pid} = sensor_nav:start(Rate, no_init),
  Pid.


% ===== SENSOR ALS =====
add_sensor_als() ->
  {ok, Pid} = sensor_als:start(),
  Pid.

add_sensor_als(no_init) ->
  {ok, Pid} = sensor_als:start(1000, no_init),
  Pid;
add_sensor_als(Rate) ->
  {ok, Pid} = sensor_als:start(Rate),
  Pid.

add_sensor_als(Rate, no_init) ->
  {ok, Pid} = sensor_als:start(Rate, no_init),
  Pid.


% ===== SENSOR GYRO =====
add_sensor_gyro() ->
  {ok, Pid} = sensor_gyro:start(),
  Pid.

add_sensor_gyro(no_init) ->
  {ok, Pid} = sensor_gyro:start(1000, no_init),
  Pid;
add_sensor_gyro(Rate) ->
  {ok, Pid} = sensor_gyro:start(Rate),
  Pid.

add_sensor_gyro(Rate, no_init) ->
  {ok, Pid} = sensor_gyro:start(Rate, no_init),
  Pid.


% ===== CONDITIONAL =====
add_conditional_proc(Predicate) ->
  {ok, Pid} = conditional:start(Predicate),
  Pid.

add_conditional_proc(Predicate, Function) ->
  {ok, Pid} = conditional:start(Predicate, Function),
  Pid.


% ===== QUEUE =====
add_queue_proc(Arg) ->
  {ok, Pid} = myqueue:start(Arg),
  Pid.


% ===== FILTER =====
add_filter_proc(Predicate) ->
  {ok, Pid} = filter:start(Predicate),
  Pid.


% ===== AGGREGATE =====
add_aggregate_proc(Function) ->
  {ok, Pid} = aggregate:start(Function),
  Pid.


% ===== MEAN =====
add_mean_proc() ->
  {ok, Pid} = aggregate:start(fun functions:mean/1),
  Pid.


% ===== MEDIAN =====
add_median_proc() ->
  {ok, Pid} = aggregate:start(fun functions:median/1),
  Pid.


% ===== MAX =====
add_max_proc() ->
  {ok, Pid} = aggregate:start(fun functions:max/1),
  Pid.


% ===== MIN =====
add_min_proc() ->
  {ok, Pid} = aggregate:start(fun functions:min/1),
  Pid.
