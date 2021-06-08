-module(file_utils).

-export([read_lines/1, print_strings/1]).


read_lines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.


get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof -> [];
    Line -> Line ++ get_all_lines(Device)
  end.


print_strings([]) ->
  ok;
print_strings([H|T]) ->
  io:format(H),
  io:format("\n"),
  print_strings(T).