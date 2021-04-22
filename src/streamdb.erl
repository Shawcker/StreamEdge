% @doc streamdb public API used to connect modules.
% @end
-module(streamdb).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([scenario_1/0, scenario_1/1]).

%--- Callbacks -----------------------------------------------------------------

%% @doc start main application
%% @TODO: accept input query and parse it
start(_Type, _Args) ->
  streamdb_sup:start_link().

stop(_State) -> ok.


loop() ->
  receive
    {value, {Value, _Timestamp}, From} ->
      io:format("Value received from process ~p: ~p~n", [From, Value]),
      loop();

    {list, List, From} ->
      io:format("List received from process ~p: ~p~n", [From, List]),
      loop();

    {trigger, _From} ->
      io:format("TRIGGER!~n"),
      loop();

    _ ->
      io:format("Unexpected message format~n"),
      loop()
  end.


% Als sensor + conditional trigger
scenario_1() ->
  Sensor_pid = add_sensor_als(),
  scenario_1_s(Sensor_pid).

scenario_1(no_init) ->
  Sensor_pid = add_sensor_als(no_init),
  scenario_1_s(Sensor_pid).

scenario_1_s(Sensor_pid) ->
  subscribe(self(), Sensor_pid),

  Below_trigger = functions:pred_filter_above(25),
  Conditional_pid = add_conditional_proc(Below_trigger),
  subscribe(Conditional_pid, Sensor_pid),

  Queue_pid = add_queue_proc(10),
  subscribe(Queue_pid, Sensor_pid),

  Mean_pid = add_mean_proc(),
  subscribe(self(), Mean_pid),

  subscribe(Mean_pid, Queue_pid),

  Send_trigger = functions:send_trigger(Conditional_pid, Queue_pid),

  Conditional_pid ! {modify, function, Send_trigger},

  loop().


scenario_2() ->
  Sensor_pid = add_sensor_als(),
  subscribe(self(), Sensor_pid),

  Queue_pid = add_queue_proc(10),
  subscribe(self(), Queue_pid),
  subscribe(Queue_pid, Sensor_pid),

  Mean_pid = add_mean_proc(),
  subscribe(Mean_pid, Queue_pid),
  subscribe(self(), Mean_pid).


scenario_3() ->
  Nav_pid = add_sensor_nav(no_init),
  subscribe(self(), Nav_pid).


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


add_queue_proc(Max_size) ->
  {ok, Pid} = myqueue:start(Max_size),
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


subscribe(Subscriber_pid, Module_pid) ->
  Module_pid ! {add_client, Subscriber_pid}.