%% @doc Module responsible for handling queues.
%% Queues are used to receive and accumulate stream data.
%% When a queue is full, it sends all its data to another module, which will compute the data.
%% Queues represent a sliding window. It has a certain maximum size, which when reached, will add new data and remove oldest data (FIFO).
%% @see functions

-module(myqueue).
-export([start/1]).
-export([start_loop/1]).

-record(queue, {queue=queue:new(), current_size=0, max_size=10, infinite=false}).
-record(clients_list, {clients=[]}).


start(infinite) ->
  Pid = spawn(?MODULE, start_loop, [infinite]),
  {ok, Pid};
%% @doc Default start function
%% @spec start(Max_size::integer()) -> {ok, Pid}
start(Max_size) ->
  Pid = spawn(?MODULE, start_loop, [Max_size]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(infinite) ->
  loop({true, none}, #queue{infinite=true}, #clients_list{});
start_loop(Max_size) ->
  loop({true, none}, #queue{max_size=Max_size}, #clients_list{}).


loop({Active, Deactivator}, Q=#queue{queue=Queue, current_size=Current_size, max_size=Max_size, infinite=Infinite}, L=#clients_list{clients=Clients}) ->
  receive

    {value, {Value, Timestamp}, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, Q, L);
        true ->
          Condition = (Current_size < Max_size-1) or Infinite,
          if
            Condition ->
              % Add to queue
              loop({Active, Deactivator}, Q#queue{queue=queue:in({Value, Timestamp}, Queue), current_size=Current_size+1}, L);
            true ->
              % Send queue, then add/remove
              to_list_and_send(Queue, Clients),
              Tmp = queue:drop(Queue),
              loop({Active, Deactivator}, Q#queue{queue=queue:in({Value, Timestamp}, Tmp), current_size=Max_size}, L)
          end
      end;


    {trigger, From} ->
      case Deactivator of
        none ->
          if
            Infinite ->
              to_list_and_send(Queue, Clients),
              loop({false, From}, #queue{infinite=true}, L);
            true ->
              loop({false, From}, Q, L)
          end;

        From ->
          loop({true, none}, Q, L);

        _ ->
          loop({Active, Deactivator}, Q, L)
      end;
      

    {modify, max_size, New_max_size} ->
      loop({Active, Deactivator}, Q#queue{max_size=New_max_size}, L);

    clear ->
      loop({Active, Deactivator}, Q#queue{queue=queue:new(), current_size=0}, L);

    restart ->
      start_loop(Max_size);

    {add_client, Pid} ->
      loop({Active, Deactivator}, Q, L#clients_list{clients=[Pid|Clients]});

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, Q, L)
  end.


%% AUXILIARY FUNCTIONS


to_list_and_send(Queue, Clients) ->
  List_to_send = queue:to_list(Queue),
  utils:send_to_clients(Clients, List_to_send, self()).