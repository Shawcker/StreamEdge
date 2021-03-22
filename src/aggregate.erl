%% @doc Module responsible for handling aggregates.
%% Several functions are used to specify which aggregate is to be computed.
%% @see functions

-module(aggregate).
-export([start/1, start/2]).
-export([start_loop/2]).

-record(buffer, {function, default_amount=10, buffer=[], size=0}).
-record(clients_list, {clients=[]}).


%% @doc Default start function. Uses a default amount of 10.
%% @spec start(Function::fun((Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()})) -> {ok, Pid}
start(Function) ->
  start(Function, 10).

%% @doc Main start function. Need to specify a default amount.
%% @spec start(Function::fun((Values::list(float()), Amount::integer()) -> {Result::float(), Number_of_values::integer()}), Amount::integer()) -> {ok, Pid}
start(Function, Amount) ->
  Pid = spawn(?MODULE, start_loop, [Function, Amount]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Function, Amount) ->
  loop(#buffer{function=Function, default_amount=Amount}, #clients_list{}).


loop(B=#buffer{function=Function, default_amount=Amount, buffer=Buffer, size=Size}, L=#clients_list{clients=Clients}) ->
  receive
    {value, {Value, Timestamp}, _From} ->
      Number_of_values = Size+1,
      case Number_of_values of
        Amount -> List = [{Value, Timestamp}|Buffer],
                  compute_and_send(Function, List, Clients),
                  loop(B#buffer{buffer=[], size=0}, L);
        _      -> loop(B#buffer{buffer=[{Value, Timestamp}|Buffer], size=Number_of_values}, L)
      end;

    {modify, amount, New_amount} ->
      loop(B#buffer{default_amount=New_amount}, L);

    {modify, function, New_function} ->
      loop(B#buffer{function=New_function}, L);

    clear ->
      loop(B#buffer{buffer=[], size=0}, L);

    restart ->
      start_loop(Amount, Function);

    {add_client, Pid} ->
      loop(B, L#clients_list{clients=[Pid|Clients]});

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop(B, L)
  end.


compute_and_send(Function, List, Clients) ->
  Value = Function(List),
  Timestamp = erlang:timestamp(),
  ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()).