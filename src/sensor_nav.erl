%% @doc Module handling pmod_nav sensor.

-module(sensor_nav).
-export([start/0, start/1, start/2]).
-export([start_loop/1]).

-record(data_rate, {rate=1000}).
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
  loop({true, none}, #clients_list{clients=[]}, #data_rate{rate=Rate}).


init() ->
  grisp:add_device(spi1, pmod_nav).


loop({Active, Deactivator}, L=#clients_list{clients=Clients}, R=#data_rate{rate=Rate}) ->
  receive
    value ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, L, R);
        true ->
          Accelerometer = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl]),
          Timestamp = erlang:timestamp(),
          ok = utils:send_to_clients(Clients, {Accelerometer, Timestamp}, self()),
          timer:sleep(Rate),
          self() ! value,
          loop({Active, Deactivator}, L, R)
      end;


    {add_client, Pid} ->
      loop({Active, Deactivator}, L#clients_list{clients=[Pid|Clients]}, R);


    {modify, rate, New_rate} ->
      loop({Active, Deactivator}, L, R#data_rate{rate=New_rate});


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, L, R);
        From -> loop({true, none}, L, R);
        _ -> loop({Active, Deactivator}, L, R)
      end;


    restart ->
      start_loop(Rate);


    _ ->
      io:format("Unexpected message format~n"),
      loop({Active, Deactivator}, L, R)
  end.
