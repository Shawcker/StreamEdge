%% @doc Module responsible for handling conditionals.
%% Executes a given function when the given conditional (predicate) is verified.
%% After function executes, waits for predicate to be false and then executes it again.
%% It works like a cycle, where the function is executed when predicate changes value.
%% @see functions

-module(conditional).
-export([start/1, start/2]).
-export([start_loop/2]).

-record(state, {predicate, function}).
-record(clients_list, {clients=[]}).


%% @doc Default start function. Need to specify predicate that will act as conditional.
%% There is a default function to be executed, and which does nothing.
%% @spec start(Predicate::fun((Value::float()) -> boolean())) -> {ok, Pid::integer()}
start(Predicate) ->
  Function = fun() -> ok end,
  Pid = spawn(?MODULE, start_loop, [Predicate, Function]),
  {ok, Pid}.


%% @doc Main start function. Need to specify predicate that will act as conditional, as well as a function to be executed.
%% @spec start(Predicate::fun((Value::float()) -> boolean()), Function::Fun) -> {ok, Pid::integer()}
start(Predicate, Function) ->
  Pid = spawn(?MODULE, start_loop, [Predicate, Function]),
  {ok, Pid}.


% @doc Starts and restarts main loop.
start_loop(Predicate, Function) ->
  loop({true, none}, start, #clients_list{}, #state{predicate=Predicate, function=Function}).


loop({Active, Deactivator}, Enable, L=#clients_list{clients=Clients}, S=#state{predicate=Predicate, function=Function}) ->
  receive

    {value, {Value, _Timestamp}, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, Enable, L, S);
        true ->
          Condition = Predicate(Value),
          case Enable of
            start ->
              if
                Condition ->
                  ok = Function(),
                  loop({Active, Deactivator}, true, L, S);
                true ->
                  loop({Active, Deactivator}, start, L, S)
              end;
            true ->
              if
                Condition ->
                  loop({Active, Deactivator}, true, L, S);
                true ->
                  ok = Function(),
                  loop({Active, Deactivator}, false, L, S)
              end;
            false ->
              if
                Condition ->
                  ok = Function(),
                  loop({Active, Deactivator}, true, L, S);
                true ->
                  loop({Active, Deactivator}, false, L, S)
              end
          end
      end;


    {modify, predicate, New_predicate} ->
      loop({Active, Deactivator}, Enable, L, S#state{predicate=New_predicate});


    {modify, function, New_function} ->
      loop({Active, Deactivator}, Enable, L, S#state{function=New_function});


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, Enable, L, S);
        From -> loop({true, none}, Enable, L, S);
        _ -> loop({Active, Deactivator}, Enable, L, S)
      end;


    restart ->
      start_loop(Predicate, Function);


    {add_client, Pid} ->
      loop({Active, Deactivator}, Enable, L#clients_list{clients=[Pid|Clients]}, S);


    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, Enable, L, S)
  end.
