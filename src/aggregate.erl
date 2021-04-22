%% @doc Module responsible for handling aggregates.
%% Several functions are used to specify which aggregate is to be computed.
%% @see functions

-module(aggregate).
-export([start/1]).
-export([start_loop/1]).

-record(clients_list, {clients=[]}).


%% @doc Main start function. Need to specify a function.
%% @spec start(Function::fun((Values::list(float())) -> float())) -> {ok, Pid}
start(Function) ->
  Pid = spawn(?MODULE, start_loop, [Function]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Function) ->
  loop({true, none}, Function, #clients_list{}).


loop({Active, Deactivator}, Function, L=#clients_list{clients=Clients}) ->
  receive

    {list, List, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, Function, L);
        true ->
          compute_and_send(Function, List, Clients),
          loop({Active, Deactivator}, Function, L)
      end;

    {modify, function, New_function} ->
      loop({Active, Deactivator}, New_function, L);

    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, Function, L);
        From -> loop({true, none}, Function, L);
        _ -> loop({Active, Deactivator}, Function, L)
      end;

    restart ->
      start_loop(Function);

    {add_client, Pid} ->
      loop({Active, Deactivator}, Function, L#clients_list{clients=[Pid|Clients]});

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, Function, L)
  end.


compute_and_send(Function, List, Clients) ->
  Value = Function(List),
  Timestamp = erlang:timestamp(),
  ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()).