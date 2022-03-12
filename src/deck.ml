exception Empty

type suit =
  | Spades
  | Diamonds
  | Hearts
  | Clubs

type card = {
  suit : suit;
  value : int;
}

let make_card suit value = { suit; value }

let rec make_suit suit value =
  match value with
  | 14 -> []
  | x ->
      if x <= 13 then make_card suit value :: make_suit suit (value + 1)
      else raise Empty

let make_deck =
  make_suit Spades 2 @ make_suit Diamonds 2 @ make_suit Hearts 2
  @ make_suit Clubs 2
