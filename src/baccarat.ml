open Ourdeck
open Constants

exception EmptyHand

type t = { winnings : int }

let winnings st = st.winnings

(*Either a valid bet or invalid bet*)
type bet_type =
  | Valid of int
  | Invalid

(*Winner of the game, either Dealer, Player or Tie*)
type winner =
  | Dealer
  | Player
  | Tie

(*Returns value of specified card*)
let bacc_score card = value_of_card card

(*returns value of all cards in specified hand*)
let rec bacc_score_hand card_list =
  match card_list with
  | [] -> raise EmptyHand
  | [ x ] -> bacc_score x
  | o :: t -> bacc_score o + bacc_score_hand t

(*Asks for user input on whether they want another card or not and
  executes based on input*)
let rec bacc_hit (x : card list) =
  print_endline "Would you like to hit? (y/n)\n";
  let new_command = read_line () in
  match new_command with
  | "y" -> (pull_card x 1, true)
  | "n" -> (pull_card x 0, false)
  | _ ->
      print_endline "Invalid input";
      bacc_hit x

(*Deals card to dealer*)
let dealer_hit (dealer_hand : card list) (current_deck : card list) =
  let x = pull_card current_deck 1 in
  dealer_hand @ x

(*Determines if dealer needs to be hit with another card and executes
  based on dealer score and if user hit*)
let dealer_hit_stuff dealer_score hit_bool dealer_hand current_deck =
  if dealer_score < 5 && hit_bool = false then
    dealer_hit dealer_hand current_deck
  else dealer_hand

(*Brings score from over 10 down to value less than 10 using mod*)
let normalize_score score = if score > 9 then score mod 10 else score

(*Determines winner of game based on player score and dealer score*)
let determine_winner dealer_hand player_hand =
  let dealer_score = normalize_score (bacc_score_hand dealer_hand) in
  let player_score = normalize_score (bacc_score_hand player_hand) in
  if player_score > dealer_score then Player
  else if player_score = dealer_score then Tie
  else Dealer

(*Gets the bet amount the user wants to bet and makes sure they have the
  money to make said bet*)
let rec get_bet balance =
  try
    let bet =
      int_of_string
        (read_line
           (print_endline "How much money would you like to bet?\n"))
    in
    if balance - bet >= 0 then Valid bet
    else (
      print_endline "Insufficient Funds. Try again.";
      get_bet balance)
  with
  | Sys_error _ ->
      print_string "Illegal dollar amount (whole dollars only).\n";
      get_bet balance
  | Failure _ ->
      print_string "Illegal dollar amount (whole dollars only).\n";
      get_bet balance

(*Prints player and dealer scores*)
let print_all_scores player_score dealer_score =
  print_endline ("Your score was: " ^ string_of_int player_score);
  print_endline ("Dealer score was: " ^ string_of_int dealer_score)

(*Gives player money based on who won the game*)
let allocate_winnings winner bet_type player_score dealer_score =
  match bet_type with
  | Valid bet -> (
      match winner with
      | Dealer ->
          print_all_scores player_score dealer_score;
          print_endline
            ("You lost " ^ string_of_int (-bet) ^ " dollars. :(");
          { winnings = -bet }
      | Player ->
          print_all_scores player_score dealer_score;
          print_endline
            ("You won " ^ string_of_int (2 * bet) ^ " dollars! :)");
          { winnings = 2 * bet }
      | Tie ->
          print_all_scores player_score dealer_score;
          print_endline
            ("Tie! You won " ^ string_of_int bet ^ " dollars!");
          { winnings = bet })
  | Invalid -> failwith "Something's really messed up."

let baccarat balance : t =
  let bet_ty = get_bet balance in
  let new_deck = make_deck in
  let player_hand = pull_card new_deck 1 in
  let current_deck = Ourdeck.deck_without_cards player_hand new_deck in
  let dealer_hand = pull_card current_deck 1 in
  let current_deck =
    Ourdeck.deck_without_cards dealer_hand current_deck
  in
  let player_score = bacc_score_hand player_hand in
  let dealer_score = bacc_score_hand dealer_hand in
  print_endline
    ("Your current card value: " ^ string_of_int player_score);
  let hit_result = bacc_hit current_deck in
  let hit_card = fst hit_result in
  let hit_bool = snd hit_result in
  let player_hand = player_hand @ hit_card in
  let current_deck = Ourdeck.deck_without_cards hit_card current_deck in
  let dealer_hand =
    dealer_hit_stuff dealer_score hit_bool dealer_hand current_deck
  in
  let player_score = normalize_score (bacc_score_hand player_hand) in
  let dealer_score = normalize_score (bacc_score_hand dealer_hand) in
  let winner = determine_winner dealer_hand player_hand in
  allocate_winnings winner bet_ty player_score dealer_score
