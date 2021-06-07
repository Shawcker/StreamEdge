-module(scenarii).

-export([start_scenario/2, launch_scenario/3]).


%%%%%%%%%%%%%%%%%%%%% MONITORING %%%%%%%%%%%%%%%%%%%%%

modify_rate(Rate) ->
  sensor ! {modify, rate, Rate}.


clear() ->
  me ! clear.


start_scenario(Number, Args) ->
  me ! {scenario, Number, Args}.


compute_mean(Relative) ->
  me ! {compute, mean, Relative}.


trigger() ->
  me ! {trigger, me}.


launch_scenario(Number, Args, StartTime) ->
  case Number of
    0 -> scenario_0(Args, StartTime);
    1 -> scenario_1(Args, StartTime);
    2 -> scenario_2(Args, StartTime);
    _ -> io:format("Scenario unavailable")
  end.


%%%%%%%%%%%%%%%%%%%%% MAIN LOOP %%%%%%%%%%%%%%%%%%%%%


tmp_loop(Pid, Data) ->
  receive
    {value, {Value, _Timestamp}, _From} ->
      io:format("Value : ~p~n", [Value]),
      tmp_loop(Pid, Data);

    {list, List, _From} ->
      io:format("List : ~p~n", [List]),
      tmp_loop(Pid, [List|Data]);

    {trigger, _From} ->
      case Pid of
        none ->
          tmp_loop(Pid, Data);
        _ ->
          Pid ! {trigger, me},
          tmp_loop(Pid, Data)
      end;

    {compute, mean, Relative} ->
      compute_data(Data, Relative),
      tmp_loop(Pid, Data);

    clear ->
      tmp_loop(Pid, []);

    _ ->
      io:format("Unhandled message~n"),
      tmp_loop(Pid, Data)
  end.


%%%%%%%%%%%%%%%%%%%%% SCENARIO 0 %%%%%%%%%%%%%%%%%%%%%


scenario_0(none, StartTime) ->
  Sensor_pid = api:add_sensor_nav(no_init),
  register(als, Sensor_pid),
  %Sensor_pid ! {trigger, me},
  %subscribe(me, Sensor_pid),

  Queue_pid = api:add_queue_proc(infinite),
  utils:subscribe(Queue_pid, Sensor_pid),
  utils:subscribe(me, Queue_pid),

  tmp_loop(Queue_pid, []);

scenario_0(Node, StartTime) ->
  utils:connect_to_node(Node),
  Nodes = nodes(),
  case Nodes of
    [] ->
      io:format("Not connected to given node~n"),
      tmp_loop(none, []);
    _ ->
      ok
  end,
  Name = lists:nth(1, nodes()),
  %SName = string:split(Name, "@"),
  {me, Name} ! {scenario, 0, none},

  Sensor_pid = api:add_sensor_als(),
  utils:subscribe(self(), Sensor_pid),

  Below_trigger = functions:pred_filter_above(25),
  Conditional_pid = api:add_conditional_proc(Below_trigger),

  utils:subscribe(Conditional_pid, Sensor_pid),
  utils:subscribe(self(), Conditional_pid),

  Send_trigger = functions:send_trigger(Conditional_pid, self()),

  Conditional_pid ! {modify, function, Send_trigger},

  tmp_loop({me, Name}, []).


%%%%%%%%%%%%%%%%%%%%% SCENARIO 1 %%%%%%%%%%%%%%%%%%%%%


scenario_1(Args, StartTime) ->
  Sensor_pid = api:add_sensor_als(),
  utils:subscribe(self(), Sensor_pid),
  register(als, Sensor_pid),
  tmp_loop(me, []).


%%%%%%%%%%%%%%%%%%%%% SCENARIO 1 %%%%%%%%%%%%%%%%%%%%%


scenario_2(_Args, _StartTime) ->
  Sensor_pid = api:add_sensor_nav(no_init),
  utils:subscribe(self(), Sensor_pid),
  register(sensor, Sensor_pid),
  tmp_loop(me, []).


%%%%%%%%%%%%%%%%%%%%% AUXILIARY FUNCTIONS %%%%%%%%%%%%%%%%%%%%%


compute_data(Data, Relative) ->
  Condition = (Relative =< 1) and (Relative > 0),
  if
    not Condition ->
      compute_data(Data, 1);
    true ->
      ok
  end,
  Fct = mean_relative(Relative),
  Tmp = lists:map(Fct, Data),

  Results = compute_mean_lists(Tmp),

  Print = print_list(Relative),

  lists:map(Print, Results).


compute_mean_lists(Lists) ->
  compute_mean_lists(Lists, []).
compute_mean_lists([], Acc) ->
  Acc;
compute_mean_lists([Head|Rest], Acc) ->
  Mean = functions:mean(Head),
  compute_mean_lists(Rest, [Mean|Acc]).


print_list(Relative) ->
  fun(List) ->
    io:format("Mean of ~p of time: ~p~n", [Relative, List])
  end.


mean_relative(Relative) ->
  fun(List) ->
    {_, First} = lists:nth(1, List),
    {_, Last} = lists:last(List),

    Total_time = Last - First,
    Mean_threshold = Total_time * Relative,
    Maximum_time = First + Mean_threshold,

    Pred = predicate_before_time(Maximum_time),

    lists:filter(Pred, List)
  end.


predicate_before_time(Maximum_time) ->
  fun({_, Timestamp}) ->
      Timestamp =< Maximum_time
  end.
