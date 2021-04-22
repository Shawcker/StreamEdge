%% @doc Module containing utilitary functions

-module(utils).
-export([send_to_clients/3]).


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