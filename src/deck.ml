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

(** A copy of a deck with a given card removed.*)
let rec remove_card deck card =
  match deck with
  | [] -> raise Empty
  | h :: t -> if h = card then t else h :: remove_card t card

(** A list of a number of random cards pulled from a deck, removing each
    card from the deck as it is pulled. *)
let rec pull_card deck number =
  match deck with
  | [] -> raise Empty
  | x ->
      let rand_card = List.nth deck (Random.int (List.length deck)) in
      let new_deck = remove_card deck rand_card in
      rand_card :: pull_card new_deck (number - 1)