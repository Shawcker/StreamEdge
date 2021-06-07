%% @doc Module responsible for handling aggregates.
%% Several functions are used to specify which aggregate is to be computed.
%% @see functions

-module(aggregate).
-export([start/1]).
-export([start_loop/1]).

-record(state, {function}).
-record(clients_list, {clients=[]}).


%% @doc Main start function. Need to specify a function.
%% @spec start(Function::fun((Values::list(float())) -> float())) -> {ok, Pid}
start(Function) ->
  Pid = spawn(?MODULE, start_loop, [Function]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Function) ->
  loop({true, none}, #clients_list{}, #state{function=Function}).


loop({Active, Deactivator}, L=#clients_list{clients=Clients}, S=#state{function=Function}) ->
  receive

    {list, List, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, L, S);
        true ->
          compute_and_send(Function, List, Clients),
          loop({Active, Deactivator}, L, S)
      end;

    {modify, function, New_function} ->
      loop({Active, Deactivator}, L, S#state{function=New_function});

    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, L, S);
        From -> loop({true, none}, L, S);
        _ -> loop({Active, Deactivator}, L, S)
      end;

    restart ->
      start_loop(Function);

    {add_client, Pid} ->
      loop({Active, Deactivator}, L#clients_list{clients=[Pid|Clients]}, S);

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, L, S)
  end.


compute_and_send(Function, List, Clients) ->
  Value = Function(List),
  %Timestamp = erlang:timestamp(),
  Timestamp = erlang:system_time(second),
  ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()).