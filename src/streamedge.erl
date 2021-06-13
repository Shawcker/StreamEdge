% @doc streamdb public API used to connect modules.
% @end
-module(streamedge).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([start_scenario/2]).

%--- Callbacks -----------------------------------------------------------------

%% @doc start main application
start(_Type, _Args) ->
  streamedge_sup:start_link(),
  register(me, self()),
  loop().


stop(_State) -> ok.


%% @doc start scenario with number Number with optional list of arguments Args
start_scenario(Number) ->
  me ! {scenario, Number, []}.
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
      scenario:launch_scenario(Number, Args, StartTime),
      loop();

    _ ->
      io:format("Unexpected message format~n"),
      loop()
  end.
