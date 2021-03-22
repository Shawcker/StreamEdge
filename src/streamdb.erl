% @doc streamdb public API used to connect modules depending on input query.
% @end
-module(streamdb).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

%% @doc start main application
%% @TODO: accept input query and parse it
start(_Type, _Args) ->
  streamdb_sup:start_link(),
  Sensor_pid = add_sensor_als(),
  subscribe(self(), Sensor_pid),

  Mean_pid = add_mean_proc(),
  subscribe(Mean_pid, Sensor_pid),
  subscribe(self(), Mean_pid),

  loop().

stop(_State) -> ok.


loop() ->
  receive
    {value, {Percentage, _Timestamp}, Pid} ->
      io:format("Value received from process ~p: ~p~n", [Pid, Percentage]),
      loop();

    {mean, {Mean, Amount}, Pid} ->
      io:format("-- Mean over ~p values received from process ~p: ~p~n", [Amount, Pid, Mean]),
      loop();

    {max, {Max, Amount}, Pid} ->
      io:format("-- Maximum of last ~p values received from process ~p: ~p~n", [Amount, Pid, Max]),
      loop();

    {min, {Min, Amount}, Pid} ->
      io:format("-- Minimum of last ~p values received from process ~p: ~p~n", [Amount, Pid, Min]),
      loop();      

    _ ->
      io:format("Unexpected message format~n"),
      loop()
  end.


add_sensor_als() ->
  {ok, Pid} = sensor_als:start(),
  Pid.


add_mean_proc() ->
  {ok, Pid} = aggregate:start(fun functions:mean/2),
  Pid.


add_median_proc() ->
  {ok, Pid} = aggregate:start(fun functions:median/2),
  Pid.


add_max_proc() ->
  {ok, Pid} = aggregate:start(fun functions:max/2),
  Pid.


add_min_proc() ->
  {ok, Pid} = aggregate:start(fun functions:min/2),
  Pid.


subscribe(Subscriber_pid, Module_pid) ->
  Module_pid ! {add_client, Subscriber_pid}.