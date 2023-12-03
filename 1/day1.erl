-module(day1).
-export([run/0, test/0]).

puzzle1(Path) ->
  L = read_input(Path),
  Ns = process_lines(L, []),
  lists:sum(Ns).

process_lines([], Results) -> lists:reverse(Results);
process_lines([H|T], Results) ->
  First = find_first_digit(H),
  Last = find_first_digit(lists:reverse(H)),
  process_lines(T, [First * 10 + Last|Results]).

find_first_digit([]) -> throw(notfound);
find_first_digit([H|T]) ->
  try erlang:list_to_integer([H]) of
    Int -> Int
  catch
    error:badarg -> find_first_digit(T)
  end.

read_input(Path) ->
  {ok, Bin} = file:read_file(Path),
  string:tokens(erlang:binary_to_list(Bin), "\n").

run() ->
  Res = puzzle1("input.txt"),
  io:format("~w~n", [Res]),
  ok.

test() ->
  142 = puzzle1("example.txt"),
  ok.
