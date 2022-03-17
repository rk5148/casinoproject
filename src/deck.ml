(*Initializes randomizer for funtion [pull_card]*)
Random.self_init ()

exception Empty

(*Type suit of the possible suits any given card can have*)
type suit =
  | Spades
  | Diamonds
  | Hearts
  | Clubs

(*Declares type of a card, requring a valid [suit] and valid [value]*)
type card = {
  suit : suit;
  value : int;
}

(*Creates a card with given [suit] and [value]*)
let make_card suit value = { suit; value }

(*Fills the given suit from card value 2 to value 14 (2 to Ace)*)
let rec make_suit suit value =
  match value with
  | 15 -> []
  | x ->
      if x <= 14 then make_card suit value :: make_suit suit (value + 1)
      else raise Empty

(*Fills the deck with cards of all suits from 2 of Spades to Ace of
  Clubs*)
let make_deck =
  make_suit Spades 2 @ make_suit Diamonds 2 @ make_suit Hearts 2
  @ make_suit Clubs 2

(** A copy of a deck with a given card removed.*)
let rec remove_card (deck : card list) (card : card) =
  match deck with
  | [] -> raise Empty
  | h :: t -> if h = card then t else h :: remove_card t card

(** A list of a number of random cards pulled from a deck, removing each
    card from the deck as it is pulled. *)
let rec pull_card deck number =
  if number >= 1 && number <= List.length deck then
    let rand_card = List.nth deck (Random.int (List.length deck)) in
    rand_card :: pull_card (remove_card deck rand_card) (number - 1)
  else []

(** Prints string representation of card to console *)
let string_of_card card =
  if card.suit = Spades then string_of_int card.value ^ " of Spades"
  else if card.suit = Diamonds then
    string_of_int card.value ^ " of Diamonds"
  else if card.suit = Hearts then
    string_of_int card.value ^ " of Hearts"
  else string_of_int card.value ^ " of Clubs"

(** Prints string representation of a deck to console *)
let rec print_deck deck =
  match deck with
  | [] -> print_string ""
  | h :: t ->
      print_string (string_of_card h);
      print_string "\n";
      print_deck t

(** Deals 7 random cards and prints string representations of all to
    console *)
let play_deck () =
  let deck = make_deck in
  let pulled_cards = pull_card deck 7 in
  print_deck pulled_cards
