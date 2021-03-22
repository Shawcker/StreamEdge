% @doc streamdb public API used to connect modules depending on input query.
% @end
-module(streamdb).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

%% @doc start main application
%% @todo accept input query and parse it
start(_Type, _Args) ->
  streamdb_sup:start_link(),
  Sensor_pid = add_sensor(),
  {ok, Mean_pid} = aggregate:start(fun functions:mean/2),

  Sensor_pid ! {add_client, Mean_pid},
  Mean_pid ! {add_client, self()},

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


add_sensor() ->
  Sensor_pid = spawn(sensor_als, start, []),
  Sensor_pid ! {add_client, self()},
  Sensor_pid.