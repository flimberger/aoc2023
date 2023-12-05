-module(gear_ratios).
-export([puzzle1/0, test1/0, puzzle2/0, test2/0]).

puzzle1() ->
  N = puzzle1("input.txt"),
  io:format("~w~n", [N]).
test1() ->
  4361 = puzzle1("example.txt"),
  0 = puzzle1("regress.txt"),
  ok.

puzzle2() ->
  N = puzzle2("input.txt"),
  io:format("~w~n", [N]).
test2() ->
  467835 = puzzle2("example.txt"),
  ok.

puzzle1(Filename) ->
  {Numbers, Symbols} = read_schematic(Filename),
  %io:format("numbers: ~w~nsymbols: ~w~n", [Numbers, Symbols]),
  PartNumbers = find_part_numbers(Numbers, Symbols, []),
  %io:format("part numbers: ~w~n", [PartNumbers]),
  lists:sum(PartNumbers).

puzzle2(Filename) ->
  {Numbers, Symbols} = read_schematic(Filename),
  GearRatios = find_gear_ratios(Numbers, lists:filter(fun({S, _, _}) ->
                                                        case S of
                                                          42 -> true;
                                                          _ -> false
                                                        end
                                                      end, Symbols), []),
  lists:sum(GearRatios).

% Read the lines into a "map" of the schematic, which is a tuple of a list of
% numbers ({Num, Line, StartCol, EndCol}) and a list of symbols ({Sym, Line, Col}).
read_schematic(Filename) ->
  Lines = read_lines(Filename),
  read_schematic(Lines, 0, {[], []}).
read_schematic([], _, Acc) -> Acc;
read_schematic([H|T], CurLine, {NumAcc, SymAcc}) ->
  {Numbers, Symbols} = read_schematic_line(H, CurLine),
  read_schematic(T, CurLine + 1, {lists:flatten([Numbers|NumAcc]),
                                  lists:flatten([Symbols|SymAcc])}).

read_schematic_line(Line, CurLine) ->
  read_schematic_line(Line, {CurLine, 0}, {[], []}).
read_schematic_line([], _, Acc) -> Acc;
read_schematic_line([H|T], {CurLine, CurCol}, Acc = {Numbers, Symbols}) ->
  case token_type(H) of
    dot -> read_schematic_line(T, {CurLine, CurCol + 1}, Acc);
    symbol -> read_schematic_line(T, {CurLine, CurCol + 1},
                                  {Numbers, [make_symbol_entry(H, CurLine, CurCol)|Symbols]});
    number -> read_number(T, {[H], CurLine, CurCol}, {CurLine, CurCol + 1}, Acc)
  end.

read_number([], {N, Line, Col}, _, {Numbers, Symbols}) ->
  {[make_number_entry(N, Line, Col)|Numbers], Symbols};
read_number([H|T], {N, Line, Col}, {CurLine, CurCol}, Acc = {Numbers, Symbols}) ->
  case token_type(H) of
    dot -> read_schematic_line(T, {CurLine, CurCol + 1},
                               {[make_number_entry(N, Line, Col)|Numbers], Symbols});
    symbol -> read_schematic_line(T,
                                  {CurLine, CurCol + 1},
				  {
                                   [make_number_entry(N, Line, Col)|Numbers],
                                   [make_symbol_entry(H, CurLine, CurCol)|Symbols]});
    number -> read_number(T, {[H|N], Line, Col}, {CurLine, CurCol + 1}, Acc)
  end.

make_symbol_entry(Sym, Line, Col) -> {Sym, Line, Col}.
make_number_entry(Chars, Line, Col) ->
  {erlang:list_to_integer(lists:reverse(Chars)), Line, Col, Col + erlang:length(Chars)}.

token_type(T) when T =:= 46 -> dot;
token_type(T) when (T >= 48) and (T =< 57) -> number;
token_type(_) -> symbol.

% until no numbers are left
find_part_numbers([], _, Acc) -> Acc;
find_part_numbers([H={Num, _, _, _}|T], Symbols, Acc) ->
  NewAcc = case is_part_number(H, Symbols) of
      true -> [Num|Acc];
      false -> Acc
    end,
  find_part_numbers(T, Symbols, NewAcc).

is_part_number(_, []) -> false;
is_part_number(N, [H|T]) ->
  case is_neighbor(N, H) of
    true -> true;
    false -> is_part_number(N, T)
  end.

is_neighbor({Num, NLine, NColStart, NColEnd}, {Sym, SLine, SCol}) ->
  LineDiff = erlang:abs(SLine - NLine),
  if
    LineDiff =:= 0 andalso (NColStart - SCol =:= 1 orelse SCol - NColEnd =:= 0) ->
      io:format("~w adjacent to ~c on line ~w~n", [Num, Sym, NLine]),
      true;
    LineDiff =:= 1 andalso SCol >= NColStart - 1 andalso SCol =< NColEnd ->
      io:format("~w on line ~w adjacent to ~c at {~w, ~w}~n", [Num, NLine, Sym, SLine, SCol]),
      true;
    true -> false
  end.

% until no gears are left
find_gear_ratios(_, [], Acc) -> Acc;
find_gear_ratios(Numbers, [H|T], Acc) ->
  N = get_neighbored_numbers(H, Numbers, []),
  NewAcc = case N of
      [] -> Acc;
      [_] -> Acc;
      [{A, _, _, _}, {B, _, _, _}] -> [A*B|Acc]
      % everything else should crash
    end,
  find_gear_ratios(Numbers, T, NewAcc).

get_neighbored_numbers(_, [], Acc) -> Acc;
get_neighbored_numbers(Gear, [H|T], Acc) ->
  NewAcc = case is_neighbor(H, Gear) of
      true -> [H|Acc];
      false -> Acc
    end,
  get_neighbored_numbers(Gear, T, NewAcc).

read_lines(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  string:tokens(erlang:binary_to_list(Bin), "\n").
