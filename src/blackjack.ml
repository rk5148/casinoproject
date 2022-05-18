open Ourdeck

type t = { winnings : int }

let winnings st = st.winnings

type bet_type =
  | Valid of int
  | Invalid

type winner =
  | Dealer
  | Player

let bk_score card = value_of_card card

let rec bk_score_hand card_list =
  match card_list with
  | [] -> min_int
  | [ x ] -> bk_score x
  | o :: t -> bk_score o + bk_score_hand t

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

let rec dealer_hit_stuff dealer_score dealer_hand current_deck =
  if dealer_score <= 16 then
    let dealt = pull_card current_deck 1 in
    let dh = dealer_hand @ dealt in
    let ds = bk_score_hand dh in
    dealer_hit_stuff ds dh
      (Ourdeck.deck_without_cards dealt current_deck)
  else dealer_hand

let determine_winner dealer_hand player_hand =
  let dealer_score = bk_score_hand dealer_hand in
  let player_score = bk_score_hand player_hand in
  if player_score > 21 then Dealer
  else if dealer_score > 21 then Player
  else if player_score > dealer_score then Player
  else Dealer

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

let print_all_scores player_score dealer_score =
  print_endline ("Your score is: " ^ string_of_int player_score);
  print_endline ("Dealer score is: " ^ string_of_int dealer_score)

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