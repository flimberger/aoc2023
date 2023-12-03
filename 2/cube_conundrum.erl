-module(cube_conundrum).
-export([puzzle1/0, test1/0]).

puzzle1() ->
  N = puzzle1("input.txt", {12, 13, 14}),
  io:format("~w~n", [N]).
test1() -> 
  8 = puzzle1("example1.txt", {12, 13, 14}),
  ok.

puzzle1(Filename, Ncubes) ->
  Lines = read_input(Filename),
  Games = parse_games(Lines, []),
  N = process_games(Games, Ncubes),
  lists:sum(N).

process_games(Games, Ncubes) -> process_games(Games, Ncubes, []).
process_games([], _, Acc) -> Acc;
process_games([{Id, Rounds}|T], Ncubes, Acc) ->
  MaxCubes = count_cubes(Rounds, {0, 0, 0}),
  NewAcc = add_if_possible(Id, MaxCubes, Ncubes, Acc),
  process_games(T, Ncubes, NewAcc).

count_cubes([], Max) -> Max;
count_cubes([{Red, Green, Blue}|T], {MaxRed, MaxGreen, MaxBlue}) ->
  NewMax = {lists:max([Red, MaxRed]), lists:max([Green, MaxGreen]), lists:max([Blue, MaxBlue])},
  count_cubes(T, NewMax).

% if the max counts are smaler than the limits, add the Id to the Accumulator
add_if_possible(Id, {Maxred, Maxgreen, Maxblue}, {Nred, Ngreen, Nblue}, Acc)
  when (Maxred =< Nred) and (Maxgreen =< Ngreen) and (Maxblue =< Nblue) -> [Id|Acc];
% otherwise, ignore it and just return the accumulator as is
add_if_possible(_, _, _, Acc) -> Acc.

parse_games([], Acc) -> Acc;
parse_games([H|T], Acc) ->
  % get game id "Game <n>:"
  [GameStr, RoundsStr] = string:split(H, ": "),
  [_, IdStr] = string:split(GameStr, " "),
  Id = erlang:list_to_integer(IdStr),
  Rounds = parse_rounds(RoundsStr, []),
  parse_games(T, [{Id, Rounds}|Acc]).

parse_rounds(Line, Acc) ->
  % rounds are separated by semicolons
  case string:split(Line, "; ") of
    [RoundStr, Remaining] ->
      Round = parse_round(RoundStr),
      parse_rounds(Remaining, [Round|Acc]);
    [Remaining] ->
      Round = parse_round(Remaining),
      [Round|Acc]
  end.

parse_round(RoundStr) -> parse_round(RoundStr, {0, 0, 0}).
parse_round(RoundStr, Ncubes) ->
  case string:split(RoundStr, ", ") of
    [CubesStr, RemRound] ->
      NewCubes = parse_cubes(CubesStr, Ncubes),
      parse_round(RemRound, NewCubes);
    [CubesStr] ->
      parse_cubes(CubesStr, Ncubes)
  end.

% parse the cound of cubes and return the given tuple with the colour set,
% e.g. 1 red -> {1, G, B}, 2 green -> {R, 2, B}, 3 blue -> {R, G, 3}
parse_cubes(CubeStr, {Nred, Ngreen, Nblue}) ->
  [Nstr, Colour] = string:split(CubeStr, " "),
  N = erlang:list_to_integer(Nstr),
  case Colour of
    "red" -> {N, Ngreen, Nblue};
    "green" -> {Nred, N, Nblue};
    "blue" -> {Nred, Ngreen, N}
  end.

read_input(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  string:tokens(erlang:binary_to_list(Bin), "\n").
