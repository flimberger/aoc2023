-module(day1).
-export([puzzle1/1, puzzle2/1, run/0, test/0]).

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

puzzle2(Filename) ->
  Lines = read_input(Filename),
  Ns = process_lines2(Lines, []),
  lists:sum(Ns).

process_lines2([], Results) -> lists:reverse(Results);
process_lines2([H|T], Results) ->
  First = find_first_number(H),
  Last = find_last_number(H),
  process_lines2(T, [First * 10 + Last|Results]).

find_first_number(L) ->
  try find_first_number(L, [])
  catch
    badarg -> throw({badarg, L})
  end.
find_first_number([], _) -> throw(badarg);
find_first_number([H|T], Acc) ->
  try erlang:list_to_integer([H])
  catch
    error:badarg ->
      NewAcc = [H|Acc],
      case check_word(lists:reverse(NewAcc)) of
        {ok, Int} -> Int;
        badarg -> find_first_number(T, NewAcc)
      end
  end.

find_last_number(L) ->
  try find_last_number(lists:reverse(L), [])
  catch
    badarg -> throw({badarg, L})
  end.
find_last_number([], _) -> throw(badarg);
find_last_number([H|T], Acc) ->
  try erlang:list_to_integer([H])
  catch
    error:badarg ->
      NewAcc = [H|Acc],
      case check_word_prefix(NewAcc) of
        {ok, Int} -> Int;
        badarg -> find_last_number(T, NewAcc)
      end
  end.

% check if the word or its suffix contain a number
check_word([]) -> badarg;
check_word(W = [_|T]) ->
  case digit_for_word(W) of
    {ok, N} -> {ok, N};
    badarg -> check_word(T)
  end.

% check if the word or its prefix contain a number
check_word_prefix(W = [H|T]) ->
  case digit_for_word(W) of
    {ok, N} -> {ok, N};
    badarg -> check_word_prefix(T, [H])
  end.
check_word_prefix([], _) -> badarg;
check_word_prefix([H|T], Acc) ->
  case digit_for_word(lists:reverse(Acc)) of
    {ok, N} -> {ok, N};
    badarg ->
      check_word_prefix(T, [H|Acc])
  end.

digit_for_word(Word) ->
  case Word of
    "one" -> {ok, 1};
    "two" -> {ok, 2};
    "three" -> {ok, 3};
    "four" -> {ok, 4};
    "five" -> {ok, 5};
    "six" -> {ok, 6};
    "seven" -> {ok, 7};
    "eight" -> {ok, 8};
    "nine" -> {ok, 9};
    _ -> badarg
  end.

run() ->
  Res = puzzle2("input.txt"),
  io:format("~w~n", [Res]),
  ok.

test() ->
  281 = puzzle2("example2.txt"),
  ok.
