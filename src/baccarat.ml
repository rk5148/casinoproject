open Ourdeck

exception EmptyHand

let score card =
  let b = Ourdeck.value_of_card card in
  match b with
  | x -> if x > 10 && x < 14 then 0 else if x = 14 then 1 else x

let score_hand card_list =
  match card_list with
  | [] -> raise EmptyHand
  | o :: t -> score o + score t

let rec hit () =
  print_endline "Would you like to hit (y/n)";
  let new_command = read_line () in
  match new_command with
  | "y" -> Ourdeck.pull_card new_deck 1
  | "n" -> ()
  | _ ->
      print_endline "Invalid input";
      hit ()

let baccarat () =
  print_endline "How much money would you like to bet?";
  let money = read_line () in
  let new_deck = Ourdeck.make_deck in
  let player_hand = Ourdeck.pull_card new_deck 1 in
  let dealer_hand = Ourdeck.pull_card new_deck 1 in
  let player_score = score_hand player_hand in
  let dealer_score = score_hand dealer_hand in
  print_endline (string_of_int player_score);
  print_endline "Would you like to hit (y/n)";

  let new_command = read_line () in
  if dealer_score > player_score then dealer_score else player_score
