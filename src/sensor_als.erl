-module(sensor_als).
-export([start/0, start/1, start/2]).

-record(data_rate, {rate=1000}).
-record(clients_list, {clients=[]}).


start() ->
  start(1000).

start(Rate) when Rate >= 1000 ->
  init(),
  start_loop(Rate);
start(Rate) when Rate < 1000 ->
  start(1000).

start(Rate, no_init) when Rate >= 1000 ->
  start_loop(Rate);
start(Rate, no_init) when Rate < 1000 ->
  start(1000, no_init).


start_loop(Rate) ->
  self() ! value,
  loop(#clients_list{clients=[]}, #data_rate{rate=Rate}).


init() ->
  grisp:add_device(spi2, pmod_als).


loop(L=#clients_list{clients=Clients}, R=#data_rate{rate=Rate}) ->
  receive
    value ->
      Percentage = pmod_als:percentage(),
      Timestamp = erlang:timestamp(),
      ok = send_to_clients(L#clients_list.clients, Percentage, Timestamp),
      timer:sleep(Rate),
      self() ! value,
      loop(L, R);

    {add_client, Pid} ->
      loop(L#clients_list{clients=[Pid|Clients]}, R);

    {modify_rate, New_rate} ->
      loop(L, R#data_rate{rate=New_rate});

    restart ->
      loop(#clients_list{}, #data_rate{});

    _ ->
      io:format("Unexpected message format~n"),
      loop(L, R)
  end.


send_to_clients([], _, _) ->
  ok;
send_to_clients([C|Rest], Percentage, Timestamp) ->
  C ! {value, {Percentage, Timestamp}, self()},
  send_to_clients(Rest, Percentage, Timestamp).
