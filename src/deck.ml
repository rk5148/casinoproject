exception Empty

type suit =
  | Spade
  | Diamond
  | Heart
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

let rec make_deck =
  make_suit Spade 0 @ make_suit Diamond 0 @ make_suit Heart 0
  @ make_suit Clubs 0
