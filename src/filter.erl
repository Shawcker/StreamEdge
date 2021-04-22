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
  loop({true, none}, #data{predicate=Predicate}, #clients_list{}).


loop({Active, Deactivator}, D=#data{predicate=Predicate}, L=#clients_list{clients=Clients}) ->
  receive

    {value, {Value, Timestamp}, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, D, L);
        true ->
          Condition = Predicate(Value),
          if
            Condition ->
              ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()),
              loop({Active, Deactivator}, D, L);
            true ->
              loop({Active, Deactivator}, D, L)
          end
      end;


    {modify, predicate, New_predicate} ->
      loop({Active, Deactivator}, D#data{predicate=New_predicate}, L);


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, D, L);
        From -> loop({true, none}, D, L);
        _ -> loop({Active, Deactivator}, D, L)
      end;


    restart ->
      start_loop(Predicate);


    {add_client, Pid} ->
      loop({Active, Deactivator}, D, L#clients_list{clients=[Pid|Clients]});


    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, D, L)
  end.
