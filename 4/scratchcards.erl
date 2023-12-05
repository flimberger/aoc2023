-module(scratchcards).
-export([puzzle1/0, test1/0]).

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

read_input(Filename) ->
  Lines = read_lines(Filename),
  read_card_pile(Lines, []).

read_card_pile([], Acc) -> Acc;
read_card_pile([H|T], Acc) ->
  Card = read_card(H),
  read_card_pile(T, [Card|Acc]).

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
