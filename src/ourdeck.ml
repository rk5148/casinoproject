(*Initializes randomizer for funtion [pull_card]*)
Random.self_init ()

exception BadValue of string
exception Empty
exception NotFound

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

let value_of_card card = card.value

let compare_card card1 card2 : int =
  if value_of_card card1 > value_of_card card2 then 1
  else if value_of_card card1 < value_of_card card2 then -1
  else if value_of_card card1 = value_of_card card2 then 0
  else raise (BadValue "Card Value Error")

let make_card suit value =
  if value > 14 || value < 2 then
    raise
      (BadValue
         "Out of scope for a deck (value must be between 2 and 14 \
          inclusive)")
  else { suit; value }

let rec make_suit suit value =
  if value < 2 || value > 15 then
    raise
      (BadValue
         "Out of scope for make_suit (value must be between 2 and 15 \
          inclusive)")
  else
    match value with
    | 15 -> []
    | x ->
        if x <= 14 then
          make_card suit value :: make_suit suit (value + 1)
        else raise Empty

let make_deck =
  make_suit Spades 2 @ make_suit Diamonds 2 @ make_suit Hearts 2
  @ make_suit Clubs 2

let rec remove_card (deck : card list) (card : card) =
  match deck with
  | [] -> deck
  | h :: t -> if h = card then t else h :: remove_card t card

let rec pull_card deck number =
  if number >= 1 && number <= List.length deck then
    let rand_card = List.nth deck (Random.int (List.length deck)) in
    rand_card :: pull_card (remove_card deck rand_card) (number - 1)
  else []

(** [is_face card] determines if the card is a face card and returns the
    correct label for the face card as a string (e.g. Ace, King, Queen,
    Jack). If the card is not a face card, this returns the string
    representation of the card's value. Requires: [card] is a valid
    card.*)
let is_face card =
  if card.value > 14 then raise (BadValue "Value greater than 14")
  else if card.value == 14 then "Ace"
  else if card.value == 13 then "King"
  else if card.value == 12 then "Queen"
  else if card.value == 11 then "Jack"
  else string_of_int card.value

let string_of_card card =
  let value = is_face card in
  if card.suit = Spades then value ^ " of Spades"
  else if card.suit = Diamonds then value ^ " of Diamonds"
  else if card.suit = Hearts then value ^ " of Hearts"
  else value ^ " of Clubs"

let rec string_of_deck deck =
  match deck with
  | [] -> ""
  | h :: t -> string_of_card h ^ "\n" ^ string_of_deck t

let play_deck () =
  let deck = make_deck in
  let pulled_cards = pull_card deck 7 in
  print_endline (string_of_deck pulled_cards)

let deck_without_cards
    (cards_list : card list)
    (current_deck : card list) =
  let c card = if List.mem card cards_list then false else true in
  List.filter c current_deck

let pull_two_compare player1 player2 =
  let deck = make_deck in
  let player1card = List.nth (pull_card deck 1) 0 in
  let player2card = List.nth (pull_card deck 1) 0 in
  print_endline ("\n" ^ player1 ^ " : " ^ string_of_card player1card);
  print_endline (player2 ^ " : " ^ string_of_card player2card ^ "\n");
  compare_card player1card player1card