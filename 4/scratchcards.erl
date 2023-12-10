-module(scratchcards).
-export([puzzle1/0, test1/0, puzzle2/0, test2/0]).

puzzle1() ->
  N = puzzle1("input.txt"),
  io:format("~w~n", [N]).
test1() ->
  13 = puzzle1("example.txt"),
  ok.

puzzle1(Filename) ->
  Pile = read_input(Filename),
  valuate_card_pile(Pile, 0).

valuate_card_pile([], Sum) -> Sum;
valuate_card_pile([H|T], Sum) ->
  S = valuate_card(H),
  valuate_card_pile(T, S + Sum).

% W => WinningNumbers, N => NumbersYouHave
valuate_card({_, W, N}) -> valuate_card(W, N, 0).
valuate_card(_, [], Value) -> Value;
valuate_card(WinningNumbers, [H|T], Value) ->
  NewValue = case lists:any(fun(N) -> N =:= H end, WinningNumbers) of
      true -> case Value of
	  0 -> 1;
	  _ -> Value * 2
        end;
      false -> Value
    end,
  valuate_card(WinningNumbers, T, NewValue).

puzzle2() ->
  N = puzzle2("input.txt"),
  io:format("~w~n", [N]).
test2() ->
  30 = puzzle2("example2.txt"),
  ok.

puzzle2(Filename) ->
  Pile = read_input(Filename),
  count_cards(Pile).

count_cards(Pile) ->
  Ncards = erlang:length(Pile),
  Cmap = make_card_map(Pile, #{}),
  Emap = evaluate_cards_in_map(1, Ncards, Cmap),
  maps:fold(fun(_, {N, _, _}, Count) -> Count + N end, 0, Emap).

% Map = #{ CardNum => {Count, Wins, Nums}}
make_card_map([], Map) -> Map;
make_card_map([{Name, Wins, Nums}|T], Map) ->
  ["Card", Numstr] = string:split(Name, " "),
  Cardnum = erlang:list_to_integer(string:trim(Numstr)),
  Newmap =  maps:put(Cardnum, {1, Wins, Nums}, Map),
  make_card_map(T, Newmap).

evaluate_cards_in_map(Cid, Ncards, Map) when Cid > Ncards -> Map;
evaluate_cards_in_map(Cid, Ncards, Map) ->
  {ok, {Count, Wins, Nums}} = maps:find(Cid, Map),
  Nwins = count_wins(Wins, Nums),
  Newmap = update_cards_in_map(Ncards, Count, Nwins, Cid, Map),
  evaluate_cards_in_map(Cid + 1, Ncards, Newmap).

count_wins(Wins, Nums) ->
  lists:foldl(fun(N, Acc) -> case lists:member(N, Wins) of
      true -> Acc + 1;
      false -> Acc
    end
  end, 0, Nums).

update_cards_in_map(Maxcid, _, _, Cid, Map) when Cid > Maxcid -> Map;
update_cards_in_map(_, _, 0, _, Map) -> Map;
update_cards_in_map(Maxcid, Count, Nwins, Cid, Map) ->
  Newcid = Cid + 1,
  {ok, {N, Wins, Nums}} = maps:find(Newcid, Map),
  Newmap = maps:update(Newcid, {N + Count, Wins, Nums}, Map),
  update_cards_in_map(Maxcid, Count, Nwins - 1, Newcid, Newmap).

% common functions

read_input(Filename) ->
  Lines = read_lines(Filename),
  read_card_pile(Lines, []).

read_card_pile([], Acc) -> Acc;
read_card_pile([H|T], Acc) ->
  Card = read_card(H),
  read_card_pile(T, [Card|Acc]).

% Card = {Name, Wins, Nums}
read_card(Line) ->
  [Name, Numbers] = string:split(Line, ": "),
  [WinningNumbers, NumbersYouHave] = string:split(Numbers, "|"),
  {Name,
   make_list(WinningNumbers),
   make_list(NumbersYouHave)}.

make_list(S) ->
  N = string:split(S, " ", all),
  lists:filtermap(fun(L) ->
                    case L of
                       [] -> false;
                       _  -> {true, erlang:list_to_integer(L)}
                     end
                   end, N).

read_lines(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  string:tokens(erlang:binary_to_list(Bin), "\n").
