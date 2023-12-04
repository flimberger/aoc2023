-module(gear_ratios).
-export([puzzle1/0, test1/0]).

puzzle1() ->
  N = puzzle1("input.txt"),
  io:format("~w~n", [N]).
test1() ->
  4361 = puzzle1("example.txt"),
  0 = puzzle1("regress.txt"),
  ok.

puzzle1(Filename) ->
  Lines = read_lines(Filename),
  {Numbers, Symbols} = read_schematic(Lines),
  %io:format("numbers: ~w~nsymbols: ~w~n", [Numbers, Symbols]),
  PartNumbers = find_part_numbers(Numbers, Symbols, []),
  %io:format("part numbers: ~w~n", [PartNumbers]),
  lists:sum(PartNumbers).

% Read the lines into a "map" of the schematic, which is a tuple of a list of
% numbers and a list of symbols.
read_schematic(Lines) -> read_schematic(Lines, 0, {[], []}).
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
  case is_part_number(H, Symbols) of
    true -> find_part_numbers(T, Symbols, [Num|Acc]);
    false -> find_part_numbers(T, Symbols, Acc)
  end.

is_part_number(_, []) -> false;
is_part_number(N={Num, NLine, NColStart, NColEnd}, [{Sym, SLine, SCol}|T]) ->
  LineDiff = erlang:abs(SLine - NLine),
  if
    LineDiff =:= 0 andalso (NColStart - SCol =:= 1 orelse SCol - NColEnd =:= 0) ->
      io:format("~w adjacent to ~c on line ~w~n", [Num, Sym, NLine]),
      true;
    LineDiff =:= 1 andalso SCol >= NColStart - 1 andalso SCol =< NColEnd ->
      io:format("~w on line ~w adjacent to ~c at {~w, ~w}~n", [Num, NLine, Sym, SLine, SCol]),
      true;
    true -> is_part_number(N, T)
  end.

read_lines(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  string:tokens(erlang:binary_to_list(Bin), "\n").
