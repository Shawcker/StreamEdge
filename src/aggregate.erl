
%% @doc Module responsible for handling aggregates.
%% Several functions are used to specify which aggregate is to be computed.
%% @see functions
%% @end

-module(aggregate).
-export([start/1, start/2]).
-export([start_loop/2]).

-record(buffer, {function, default_amount=10, buffer=[], size=0, acc=0}).
-record(clients_list, {clients=[]}).


%% @doc Default start function. Uses a default amount of 10.
%% @spec start(fun((Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()})) -> {ok, Pid}
start(Function) ->
  start(10, Function).

%% @doc Main start function. Need to specify a default amount.
%% @spec start(Amount::integer(), fun((Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()})) -> {ok, Pid}
start(Amount, Function) ->
  Pid = spawn(?MODULE, start_loop, [Amount, Function]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Amount, Function) ->
  loop(#buffer{function=Function, default_amount=Amount}, #clients_list{}).


loop(B=#buffer{function=Function, default_amount=Amount, buffer=Buffer, size=Size, acc=Acc}, L=#clients_list{clients=Clients}) ->
  receive
    {value, {Value, _Timestamp}, _} ->
      case Acc of
        Amount -> self() ! {send, Amount},
                  loop(B#buffer{buffer=[Value|Buffer], acc=1, size=Size+1}, L);
        _      -> loop(B#buffer{buffer=[Value|Buffer], acc=Acc+1, size=Size+1}, L)
      end;

    {send, Amount_to_send} ->
      {Value, Number_of_values} = Function(Buffer, Amount_to_send),
      ok = send_to_clients(Clients, Value, Number_of_values),
      loop(B, L);

    {modify, amount, New_amount} ->
      loop(B#buffer{default_amount=New_amount}, L);

    {modify, function, New_function} ->
      loop(B#buffer{function=New_function}, L);

    clear ->
      loop(B#buffer{buffer=[], size=0, acc=0}, L);

    restart ->
      start_loop(Amount, Function);

    {add_client, Pid} ->
      loop(B, L#clients_list{clients=[Pid|Clients]});

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop(B, L)
  end.


send_to_clients([], _, _) ->
  ok;
send_to_clients([C|Rest], Mean, Amount) ->
  C ! {mean, {Mean, Amount}, self()},
  send_to_clients(Rest, Mean, Amount).