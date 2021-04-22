%% @doc Module responsible for handling conditionals.
%% Executes a given function when the given conditional (predicate) is verified.
%% After function executes, waits for predicate to be false and then executes it again.
%% It works like a cycle, where the function is executed when predicate changes value.
%% @see functions

-module(conditional).
-export([start/1, start/2]).
-export([start_loop/2]).

-record(data, {predicate, function, rate=1000}).
-record(clients_list, {clients=[]}).


%% @doc Default start function. Need to specify predicate that will act as conditional.
%% There is a default function to be executed.
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
  loop({true, none}, start, #data{predicate=Predicate, function=Function}, #clients_list{}).


loop({Active, Deactivator}, Enable, D=#data{predicate=Predicate, function=Function}, L=#clients_list{clients=Clients}) ->
  receive

    {value, {Value, _Timestamp}, _From} ->
      if
        not Active ->
          % Module is deactivated, ignore message
          loop({Active, Deactivator}, Enable, D, L);
        true ->
          Condition = Predicate(Value),
          case Enable of
            start ->
              if
                Condition ->
                  ok = Function(),
                  loop({Active, Deactivator}, true, D, L);
                true ->
                  loop({Active, Deactivator}, start, D, L)
              end;
            true ->
              if
                Condition ->
                  loop({Active, Deactivator}, true, D, L);
                true ->
                  ok = Function(),
                  loop({Active, Deactivator}, false, D, L)
              end;
            false ->
              if
                Condition ->
                  ok = Function(),
                  loop({Active, Deactivator}, true, D, L);
                true ->
                  loop({Active, Deactivator}, false, D, L)
              end
          end
      end;


    {modify, predicate, New_predicate} ->
      loop({Active, Deactivator}, Enable, D#data{predicate=New_predicate}, L);


    {modify, function, New_function} ->
      loop({Active, Deactivator}, Enable, D#data{function=New_function}, L);


    {trigger, From} ->
      case Deactivator of
        none -> loop({false, From}, Enable, D, L);
        From -> loop({true, none}, Enable, D, L);
        _ -> loop({Active, Deactivator}, Enable, D, L)
      end;


    restart ->
      start_loop(Predicate, Function);


    {add_client, Pid} ->
      loop({Active, Deactivator}, Enable, D, L#clients_list{clients=[Pid|Clients]});


    _ ->
      io:format("~p (Pid ~p) received an unexpected message~n", [?MODULE, self()]),
      loop({Active, Deactivator}, Enable, D, L)
  end.
