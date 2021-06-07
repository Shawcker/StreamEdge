%% @doc Module responsible for handling filtering.
%% Several functions/predicates are used to specify how the filtering is done.
%% @see functions

-module(filter).
-export([start/1]).
-export([start_loop/1]).

-record(state, {predicate, rate=1000}).
-record(clients_list, {clients=[]}).


%% @doc Main start function. Need to specify predicate that will filter incoming data.
%% @spec start(Predicate::fun((Value::float()) -> boolean())) -> {ok, Pid::integer()}
start(Predicate) ->
  Pid = spawn(?MODULE, start_loop, [Predicate]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Predicate) ->
  loop({true, none}, #clients_list{}, #state{predicate=Predicate}).


loop({Active, Deactivator}, L=#clients_list{clients=Clients}, S=#state{predicate=Predicate}) ->
  receive

    {value, {Value, Timestamp}, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, L, S);
        true ->
          Condition = Predicate(Value),
          if
            Condition ->
              ok = utils:send_to_clients(Clients, {Value, Timestamp}, self()),
              loop({Active, Deactivator}, L, S);
            true ->
              loop({Active, Deactivator}, L, S)
          end
      end;


    {modify, predicate, New_predicate} ->
      loop({Active, Deactivator}, L, S#state{predicate=New_predicate});


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, L, S);
        From -> loop({true, none}, L, S);
        _ -> loop({Active, Deactivator}, L, S)
      end;


    restart ->
      start_loop(Predicate);


    {add_client, Pid} ->
      loop({Active, Deactivator}, L#clients_list{clients=[Pid|Clients]}, S);


    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, L, S)
  end.
