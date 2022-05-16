(*Initializes randomizer for funtion [pull_card]*)
Random.self_init ()

exception BadValue of string
exception Empty
exception NotFound
exception EmptyHand (*for baccarat*)

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

(*STARTING BACCARAT RIGHT HERE IDIOTS*)

let bacc_score card =
  let b = value_of_card card in
  match b with
  | x -> if x > 10 && x < 14 then 0 else if x = 14 then 1 else x

let rec bacc_score_hand card_list =
  match card_list with
  | [] -> raise EmptyHand
  | o :: t -> bacc_score o + bacc_score_hand t

let rec bacc_hit (x : card list) =
  print_endline "Would you like to hit (y/n)";
  let new_command = read_line () in
  match new_command with
  | "y" -> (pull_card x 1, true)
  | "n" -> (pull_card x 0, false)
  | _ ->
      print_endline "Invalid input";
      bacc_hit x

let dealer_hit (dealer_hand : card list) (current_deck : card list) =
  let x = pull_card current_deck 1 in
  dealer_hand @ x

let hit_result_card hit_result =
  match hit_result with
  | x, y -> x

let hit_result_bool hit_result =
  match hit_result with
  | x, y -> y

let dealer_hit_stuff dealer_score hit_bool dealer_hand current_deck =
  if dealer_score < 5 && hit_bool = false then
    dealer_hit dealer_hand current_deck
  else dealer_hand

(* let baccarat () = print_endline "How much money would you like to
   bet?"; let money = read_line () in let new_deck = make_deck in let
   player_hand = pull_card new_deck 1 in let current_deck =
   deck_without_cards player_hand new_deck in let dealer_hand =
   pull_card current_deck 1 in let current_deck = deck_without_cards
   dealer_hand current_deck in let player_score = bacc_score_hand
   player_hand in let dealer_score = bacc_score_hand dealer_hand in
   print_endline (string_of_int player_score); let hit_result = bacc_hit
   current_deck in let hit_card = hit_result_card hit_result in let
   hit_bool = hit_result_bool hit_result in let new_player_hand =
   (player_hand @ hit_card) in let current_deck = deck_without_cards
   hit_card current_deck in let dealer_hand = dealer_hit_stuff
   dealer_score hit_bool dealer_hand current_deck in let current_deck =
   deck_without_cards dealer_hand current_deck in (*may remove card that
   is not in the deck *) *)
