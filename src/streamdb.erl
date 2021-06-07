% @doc streamdb public API used to connect modules.
% @end
-module(streamdb).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([start_scenario/2]).

%--- Callbacks -----------------------------------------------------------------

%% @doc start main application
start(_Type, _Args) ->
  streamdb_sup:start_link(),
  register(me, self()),
  loop().


stop(_State) -> ok.


start_scenario(Number, Args) ->
  me ! {scenario, Number, Args}.


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

    {scenario, Number, Args} ->
      io:format("Launching scenario ~p~n", [Number]),
      StartTime = erlang:system_time(second),
      scenarii:launch_scenario(Number, Args, StartTime);

    _ ->
      io:format("Unexpected message format~n"),
      loop()
  end.
