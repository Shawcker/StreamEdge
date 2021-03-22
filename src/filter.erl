%% @doc Module responsible for handling filtering.
%% Several functions are used to specify how the filtering is done.
%% @see functions

-module(filter).
-export([start/1]).
-export([start_loop/1]).

-record(data, {predicate, rate=1000}).
-record(clients_list, {clients=[]}).


%% @doc Main start function. Need to specify predicate that will filter incoming data.
%% @spec start(Predicate::fun((Value::float()) -> boolean())) -> {ok, Pid::integer()}
start(Predicate) ->
  Pid = spawn(?MODULE, start_loop, [Predicate]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Predicate) ->
  loop(#data{predicate=Predicate}, #clients_list{}).


loop(D=#data{predicate=Predicate}, L=#clients_list{clients=Clients}) ->
  receive
    {value, {Value, Timestamp}, _From} ->
      Condition = Predicate(Value),
      if
        Condition ->
          ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()),
          loop(D, L);
        true ->
          loop(D, L)
      end;

    {modify, predicate, New_predicate} ->
      loop(D#data{predicate=New_predicate}, L);

    restart ->
      start_loop(Predicate);

    {add_client, Pid} ->
      loop(D, L#clients_list{clients=[Pid|Clients]});

    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop(D, L)
  end.
