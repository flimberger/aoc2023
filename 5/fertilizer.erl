-module(fertilizer).
-export([test/0, puzzle1/0]).

puzzle1() ->
  N = puzzle1("input.txt"),
  io:format("~w~n", [N]).
test1() ->
  35 = puzzle1("example.txt"),
  ok.

puzzle1(Filename) ->
  {Seeds, Maps} = read_almanac(Filename),
  Locations = lists:map(fun(S) -> find_location(Maps, S) end, Seeds),
  lists:min(Locations).

read_almanac(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  [H|T] = string:tokens(erlang:binary_to_list(Bin), "\n"),
  Seeds = parse_seeds(H),
  Maps = parse_maps(T),
  {Seeds, Maps}.

parse_seeds(Line) ->
  ["seeds:"|Numstrs] = string:split(Line, " ", all),
  Seeds = lists:map(fun(S) -> erlang:list_to_integer(S) end, Numstrs),
  Seeds.

parse_maps(Lines) ->
  {_, Maps} = lists:foldl(fun(Line, {Curmap, Maps}) ->
      case string:find(Line, " map:", trailing) of
	nomatch -> % data line
	  Strs = string:split(Line, " ", all),
	  [Dest, Src, Len] = lists:map(fun(S) -> erlang:list_to_integer(string:trim(S)) end, Strs),
	  L = maps:get(Curmap, Maps, []),
	  Newmap = maps:put(Curmap, [{Dest, Src, Len}|L], Maps),
	  {Curmap, Newmap};
	_ ->
	  [Name, _] = string:split(Line, " "),
	  Newcur = case Name of
            "seed-to-soil" -> seed2soil;
            "soil-to-fertilizer" -> soil2fertilizer;
            "fertilizer-to-water" -> fertilizer2water;
            "water-to-light" -> water2light;
            "light-to-temperature" -> light2temperature;
            "temperature-to-humidity" -> temperature2humidity;
            "humidity-to-location" -> humidity2location
  	  end,
	  {Newcur, Maps}
      end
    end, {nil, #{}}, Lines),
  Maps.

map(N, []) -> N;
map(N, [{Dest, Src, Len}|T]) ->
  if
    N >= Src, N < Src + Len -> Dest + N - Src;
    true -> map(N, T)
  end.

find_location(Maps, Seed) ->
  lists:foldl(fun(K, V) ->
      M = maps:get(K, Maps),
      map(V, M)
    end, Seed, [seed2soil, soil2fertilizer, fertilizer2water, water2light, light2temperature, temperature2humidity, humidity2location]).

% tests

test() ->
  {_, Maps} = read_almanac("example.txt"),
  ok = test_map(Maps),
  ok = test_find_location(Maps),
  test1().

test_map(Maps) ->
  Soil = maps:get(seed2soil, Maps),
  81 = map(79, Soil),
  14 = map(14, Soil),
  57 = map(55, Soil),
  13 = map(13, Soil),
  ok.

test_find_location(Maps) ->
  82 = find_location(Maps, 79),
  43 = find_location(Maps, 14),
  86 = find_location(Maps, 55),
  35 = find_location(Maps, 13),
  ok.
