%% @doc Module containing utilitary functions

-module(utils).
-export([send_to_clients/3, connect_to_node/1, register_self/1, get_hostname/0, get/1, subscribe/2]).


%% @doc Send tuple {Value, Timestamp} to the list of 'Clients' where each Client is a process Pid
%% spec send_to_clients(Clients::list(integer()), Message::{Value, Timestamp}, Sender::integer()) -> ok
send_to_clients(Clients, {Value, Timestamp}, Sender) ->
  Send = fun(Client) -> Client ! {value, {Value, Timestamp}, Sender} end,
  lists:map(Send, Clients),
  ok;

%% @doc Send List to the list of 'Clients' where each Client is a process Pid
%% spec send_to_clients(Clients::list(integer()), Message::{Value, Timestamp}, Sender::integer()) -> ok
send_to_clients(Clients, List, Sender) ->
  Send = fun(Client) -> Client ! {list, List, Sender} end,
  lists:map(Send, Clients),
  ok.


connect_to_node(Node) ->
  net_adm:ping(Node).


register_self(Name) ->
  register(Name, self()).


%% @doc Retrieve hostname
get_hostname() ->
  case inet:gethostname() of
    {ok, Hostname} -> Hostname
  end.

%% @doc Retrieve the given environment variable
get(Key) ->
  case application:get_env(streamdb, Key) of
    {ok, Value} -> Value
  end.

%% @doc Add Module_pid to the list of clients of Subscriber_pid
subscribe(Subscriber_pid, Module_pid) ->
  Module_pid ! {add_client, Subscriber_pid}.