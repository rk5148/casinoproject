(*Open all relevant files (game files)*)
(* open Yojson.Basic.Util *)
open Player
open Wheeloffortune
open Slots
open Deck
open State
open Command

let rec string_list_to_string str =
  match str with
  | [] -> ""
  | [ x ] -> String.lowercase_ascii x
  | h :: t -> String.lowercase_ascii h ^ " " ^ string_list_to_string t

let all_game_names = [ "wheeloffortune"; "slots"; "Deal 7" ]

let wof_commands =
  [
    "1";
    "1 ";
    "Wheel of Fortune";
    "wheel of fortune";
    "WHEEL OF FORTUNE";
    "Wheel of Fortune ";
    "wheel of fortune ";
    "WHEEL OF FORTUNE ";
    "wof";
    "WOF";
    "wof ";
    "WOF ";
  ]

let slots_commands =
  [ "2"; "2 "; "Slots"; "slots"; "SLOTS"; "Slots "; "slots "; "SLOTS " ]

let deal7_commands =
  [
    "3";
    "3 ";
    "Deal 7";
    "deal 7";
    "DEAL 7";
    "Deal 7 ";
    "deal 7 ";
    "DEAL 7 ";
    "Deal7";
    "deal7";
    "DEAL7";
  ]

let get_command_from_user state =
  print_string
    "What game do you want to play? \n\
    \ (1): Wheel of Fortune, \n\
    \ (2): Slots, \n\
    \ (3): Deal 7\n\n\
    \    > ";
  read_line ()

let check_bet state bet =
  if bet <= State.bank state then true else false

let get_bet_from_user state =
  print_string
    ("How much do you want to bet? You can bet up to "
    ^ string_of_int (State.bank state)
    ^ "\n");
  let bet = int_of_string (read_line ()) in
  if check_bet state bet then bet else -1

let find_and_play_game state name =
  if name = "wheel of fortune" then Wheeloffortune.wheel_of_fortune ()
  else if name = "slots" then Slots.slots ()
  else if name = "deal 7" then Deck.play_deck ()
  else print_string "INVALID INPUT"

let rec play state =
  let command = get_command_from_user state in
  match Command.parse command with
  | Quit ->
      print_string "THANKS FOR PLAYING :)\n";
      Stdlib.exit 0
  | Family ->
      print_string ("Your familial status: " ^ State.family state ^ "\n")
  | Bank ->
      print_string
        ("Your bank: " ^ string_of_int (State.bank state) ^ "\n")
  | Play p ->
      let bet = get_bet_from_user state in
      if bet = -1 then print_string "Illegal bet, try again"
      else find_and_play_game state (string_list_to_string p)

(*Runs the game*)
let rec main () =
  print_string
    ("\n\nWelcome to The Casino!\n" ^ "Please enter your name:\n>");
  match read_line () with
  | exception End_of_file -> ()
  | name -> (
      print_string
        "\n\
         Please enter the amount of money you will be bringing into \
         the casino:\n\
         >";
      match read_line () with
      | exception End_of_file -> ()
      | start_money -> (
          try play (State.init_state name (int_of_string start_money))
          with Sys_error f ->
            print_string "Illegal money entry, please restart game."))

let () = main ()