(*Initializes randomizer for funtion [pull_card]*)
Random.self_init ()

exception Empty
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

let deck_without_cards (cards_list : card list) (current_deck : card list)=
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
  | _ -> print_endline "Invalid input"; bacc_hit x 

let dealer_hit (dealer_hand : card list) (current_deck : card list) =
  let x = pull_card current_deck 1 in dealer_hand @ x

let hit_result_card hit_result =
  match hit_result with
  | (x, y) -> x
let hit_result_bool hit_result =
  match hit_result with
  | (x, y) -> y

let dealer_hit_stuff dealer_score hit_bool dealer_hand current_deck =
  if (dealer_score < 5) && (hit_bool = false) 
  then dealer_hit dealer_hand current_deck
  else dealer_hand


let baccarat () =
  print_endline "How much money would you like to bet?";
  let money = read_line () in
  let new_deck = make_deck in
  let player_hand = pull_card new_deck 1 in
  let current_deck = deck_without_cards player_hand new_deck in 
  let dealer_hand = pull_card current_deck 1 in
  let current_deck = deck_without_cards dealer_hand current_deck in
  let player_score = bacc_score_hand player_hand in
  let dealer_score = bacc_score_hand dealer_hand in
  print_endline (string_of_int player_score);
  let hit_result = bacc_hit current_deck in
  let hit_card = hit_result_card hit_result in 
  let hit_bool = hit_result_bool hit_result in
  let new_player_hand = (player_hand @ hit_card) in
  let current_deck = deck_without_cards hit_card current_deck in
  let dealer_hand = dealer_hit_stuff dealer_score hit_bool dealer_hand current_deck in
  let current_deck = deck_without_cards dealer_hand current_deck in
    (*may remove card that is not in the deck *)
  
  



  