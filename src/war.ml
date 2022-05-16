open Ourdeck
open Constants

exception InvalidBet
exception DealingError

let minimumbet = 5

type t = { winnings : int }

let winnings state = state.winnings

type bet =
  | Primary
  | Tie
  | War
  | Surrender

type totalbet = {
  primary : int;
  tie : int;
  war : int * bet;
}

let after_bet balance primary tie war = balance - primary - tie - war

let rec get_primary_bet balance =
  let primary =
    int_of_string (read_line (print_endline "Primary Bet?"))
  in
  if after_bet balance 0 0 0 > minimumbet then
    { primary; tie = 0; war = (0, War) }
  else (
    print_endline "Insufficient Funds. Try again.";
    get_primary_bet balance)

let rec get_tie_bet balance primary =
  let tie = int_of_string (read_line (print_endline "Tie Bet?")) in
  if after_bet balance primary 0 0 > minimumbet && primary >= minimumbet
  then { primary; tie; war = (0, War) }
  else if primary < minimumbet then (
    print_endline
      ("Your primary bet must be at least "
      ^ string_of_int minimumbet
      ^ ".\n");
    get_tie_bet balance primary)
  else (
    print_endline "Insufficient Funds. Try again.";
    get_tie_bet balance primary)

let rec get_war_bet balance primary tie =
  let war = int_of_string (read_line (print_endline "\nWar Bet?")) in
  if
    after_bet balance primary tie 0 > minimumbet
    && war >= primary && war >= minimumbet
  then { primary; tie; war = (war, War) }
  else if war < minimumbet then (
    print_endline
      ("Your war bet must be at least "
      ^ string_of_int minimumbet
      ^ ".\n");
    get_war_bet balance primary tie)
  else if war < primary then (
    print_endline
      "Your war bet must be at least as much as your initial bet.";
    get_war_bet balance primary tie)
  else (
    print_endline "Insufficient Funds. Try again.";
    get_war_bet balance primary tie)

let get_bet (kind : bet) balance primary tie war : totalbet =
  match kind with
  | Primary -> get_primary_bet balance
  | Tie -> get_tie_bet balance primary
  | War -> get_war_bet balance primary tie
  | Surrender -> raise InvalidBet

let rec process_tie_response balance currentbet =
  let response =
    read_line (print_endline "Would you like to place a Tie Bet?")
  in
  if List.mem response Constants.yes_commands then (
    currentbet := get_bet Tie balance !currentbet.primary 0 0;
    !currentbet)
  else if List.mem response Constants.no_commands then !currentbet
  else (
    print_endline "Invalid Response.";
    process_tie_response balance currentbet)

let initial_betting balance : totalbet =
  let currentbet = ref (get_bet Primary balance 0 0 0) in
  if after_bet balance !currentbet.primary 0 0 <> 0 then
    process_tie_response balance currentbet
  else !currentbet

let rec process_war_response balance currentbet : totalbet =
  let response =
    read_line (print_endline "Would you like to Go to War?\n")
  in
  if List.mem response Constants.no_commands then
    print_endline "\nThanks for playing!";
  if List.mem response Constants.yes_commands then
    get_bet War balance currentbet.primary currentbet.tie 0
  else if List.mem response Constants.no_commands then
    {
      primary = currentbet.primary;
      tie = currentbet.tie;
      war = (0, Surrender);
    }
  else (
    print_endline "\nInvalid Response.";
    process_war_response balance currentbet)

let rec war_betting balance (currentbet : totalbet) =
  process_war_response balance currentbet

let war_card_dealing currentbet : int =
  if snd currentbet.war = Surrender then currentbet.primary / 2
  else
    let comparecard = pull_two_compare "You" "Dealer" in
    let war_bet = fst currentbet.war in
    let combined = fst currentbet.war + currentbet.tie in
    if comparecard = 1 then (
      print_endline ("You won " ^ string_of_int combined);
      war_bet)
    else if comparecard = -1 then (
      print_endline ("You lost " ^ string_of_int war_bet);
      -war_bet)
    else if comparecard = 0 then (
      print_endline ("You won " ^ string_of_int (2 * war_bet));
      2 * war_bet)
    else raise DealingError

let initial_card_dealing balance currentbet : t =
  let comparecard = pull_two_compare "You" "Dealer" in
  let primary = currentbet.primary in
  if comparecard = 1 then (
    let difference = primary - currentbet.tie in
    print_endline ("You won " ^ string_of_int difference ^ "\n");
    { winnings = difference })
  else if comparecard = -1 then (
    let combined = primary + currentbet.tie in
    print_endline ("You lost " ^ string_of_int combined ^ "\n");
    { winnings = -combined })
  else if comparecard = 0 then (
    let tie_winnings = 10 * currentbet.tie in
    print_endline
      ("It's a tie! You just won " ^ string_of_int tie_winnings ^ "\n");
    if currentbet.tie = 0 then { winnings = 0 }
    else
      {
        winnings =
          tie_winnings
          + war_card_dealing (war_betting balance currentbet);
      })
  else raise DealingError

let war balance : t =
  print_endline "Welcome to War.";
  print_endline Constants.war_help_string;
  let currentbet = initial_betting balance in
  initial_card_dealing balance currentbet
