open Ourdeck

type t = { winnings : int }

let winnings st = st.winnings

(**Either a valid bet or invalid bet*)
type bet_type =
  | Valid of int
  | Invalid

(**Winner of the game, either Dealer or Player*)
type winner =
  | Dealer
  | Player

(**Returns value of specified card*)
let bk_score card = value_of_card card

(**returns value of all cards in specified hand*)
let rec bk_score_hand card_list =
  match card_list with
  | [] -> min_int
  | [ x ] -> bk_score x
  | o :: t -> bk_score o + bk_score_hand t

(**Asks for user input on whether they want another card or not and
   executes based on input*)
let rec bk_hit (x : card list) =
  print_endline "Would you like to hit? (y/n)\n";
  let new_command = read_line () in
  match new_command with
  | "y" ->
      let pulled = pull_card x 1 in
      pulled @ bk_hit (Ourdeck.deck_without_cards pulled x)
  | "n" -> pull_card x 0
  | _ ->
      print_endline "Invalid input";
      bk_hit x

(**Determines if dealer needs to be hit with another card and executes
   based on dealer score and if user hit*)
let rec dealer_hit_stuff dealer_score dealer_hand current_deck =
  if dealer_score <= 16 then
    let dealt = pull_card current_deck 1 in
    let dh = dealer_hand @ dealt in
    let ds = bk_score_hand dh in
    dealer_hit_stuff ds dh
      (Ourdeck.deck_without_cards dealt current_deck)
  else dealer_hand

(**Determines winner of game based on player score and dealer score*)
let determine_winner dealer_hand player_hand =
  let dealer_score = bk_score_hand dealer_hand in
  let player_score = bk_score_hand player_hand in
  if player_score > 21 then Dealer
  else if dealer_score > 21 then Player
  else if player_score > dealer_score then Player
  else Dealer

(**Gets the bet amount the user wants to bet and makes sure they have
   the money to make said bet*)
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

(**Prints player and dealer scores*)
let print_all_scores player_score dealer_score =
  print_endline ("Your score is: " ^ string_of_int player_score);
  print_endline ("Dealer score is: " ^ string_of_int dealer_score)

(**Gives player money based on who won the game*)
let allocate_winnings winner bet_type player_score dealer_score =
  match bet_type with
  | Valid bet -> (
      match winner with
      | Dealer ->
          print_all_scores player_score dealer_score;
          print_endline
            ("You lost " ^ string_of_int bet ^ " dollars. :(");
          { winnings = -bet }
      | Player ->
          print_all_scores player_score dealer_score;
          print_endline
            ("You won " ^ string_of_int (2 * bet) ^ " dollars! :)");
          { winnings = 2 * bet })
  | Invalid -> failwith "Something's really messed up."

(**Play game of balckjack*)
let blackjack balance : t =
  let bet_ty = get_bet balance in
  let new_deck = make_deck in
  let player_hand = pull_card new_deck 2 in
  let current_deck = Ourdeck.deck_without_cards player_hand new_deck in
  let dealer_hand = pull_card current_deck 1 in
  let current_deck =
    Ourdeck.deck_without_cards dealer_hand current_deck
  in
  let player_score = bk_score_hand player_hand in
  let dealer_score = bk_score_hand dealer_hand in
  print_all_scores player_score dealer_score;
  let dealer_hand = dealer_hand @ pull_card current_deck 1 in
  let current_deck =
    Ourdeck.deck_without_cards dealer_hand current_deck
  in
  let dealer_score = bk_score_hand dealer_hand in
  let hit_cards = bk_hit current_deck in
  let current_deck =
    Ourdeck.deck_without_cards hit_cards current_deck
  in
  let player_hand = player_hand @ hit_cards in
  let dealer_hand =
    dealer_hit_stuff dealer_score dealer_hand current_deck
  in
  let player_score = bk_score_hand player_hand in
  let dealer_score = bk_score_hand dealer_hand in
  let winner = determine_winner dealer_hand player_hand in
  allocate_winnings winner bet_ty player_score dealer_score