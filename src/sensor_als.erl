%% @doc Module handling pmod_als sensor.

-module(sensor_als).
-export([start/0, start/1, start/2]).
-export([start_loop/1]).

-record(state, {rate=1000}).
-record(clients_list, {clients=[]}).


%% @doc Default start function. Uses a default rate of 1 per second.
start() ->
  start(1000).

%% @doc Main start function. Need to specify a data rate.
start(Rate) when Rate >= 1000 ->
  init(),
  Pid = spawn(?MODULE, start_loop, [Rate]),
  {ok, Pid};
start(Rate) when Rate < 1000 ->
  start(1000).

%% @doc Special start function. Used when no need to init i.e. sensor has already been added (testing purposes).
start(Rate, no_init) when Rate >= 1000 ->
  Pid = spawn(?MODULE, start_loop, [Rate]),
  {ok, Pid};
start(Rate, no_init) when Rate < 1000 ->
  start(1000, no_init).


start_loop(Rate) ->
  self() ! value,
  loop({true, none}, #clients_list{clients=[]}, #state{rate=Rate}).


init() ->
  grisp:add_device(spi2, pmod_als).


loop({Active, Deactivator}, L=#clients_list{clients=Clients}, S=#state{rate=Rate}) ->
  receive
    value ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, L, S);
        true ->
          Percentage = pmod_als:percentage(),
          Timestamp = erlang:system_time(second),
          ok = utils:send_to_clients(Clients, {Percentage, Timestamp}, self()),
          timer:sleep(Rate),
          self() ! value,
          loop({Active, Deactivator}, L, S)
      end;


    {add_client, Pid} ->
      loop({Active, Deactivator}, L#clients_list{clients=[Pid|Clients]}, S);


    {modify, rate, New_rate} ->
      loop({Active, Deactivator}, L, S#state{rate=New_rate});


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, L, S);
        From -> loop({true, none}, L, S);
        _ -> loop({Active, Deactivator}, L, S)
      end;


    restart ->
      loop({Active, Deactivator}, #clients_list{}, #state{});


    _ ->
      io:format("Unexpected message format~n"),
      loop({Active, Deactivator}, L, S)
  end.
