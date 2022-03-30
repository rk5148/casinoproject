(*Open all relevant files (game files)*)
(* open Yojson.Basic.Util *)
(* open Wheeloffortune *)
(* open Slots *)
(* open Deck *)
open State
open Command

let rec string_list_to_string str =
  match str with
  | [] -> ""
  | [ x ] -> String.lowercase_ascii x
  | h :: t -> String.lowercase_ascii h ^ " " ^ string_list_to_string t

let all_games = [ "wheel of fortune"; "slots"; "craps" ]

let get_command_from_user state =
  print_string
    "\n\
     What game do you want to play? \n\
    \ (1): Wheel of Fortune\n\
    \ (2): Slots\n\
    \ (3): Craps\n\n\
    \    > ";
  read_line ()

let check_bet state bet =
  if bet <= State.balance state then true else false

let get_bet_from_user state =
  print_string
    ("How much do you want to bet? You can bet up to "
    ^ string_of_int (State.balance state)
    ^ "\n");
  let bet = int_of_string (read_line ()) in
  if check_bet state bet then bet else -1

let rec string_list_to_string lst divider =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | h :: t ->
      if h <> "" then h ^ divider ^ string_list_to_string t divider
      else string_list_to_string t divider

let rec play state =
  let continue = State.play_st state in
  match continue with
  | Lose l -> print_string l
  | Continue -> (
      let command = get_command_from_user state in
      match Command.parse command with
      | exception Command.Malformed ->
          print_string
            "\nInvalid input. Try again by typing \"play\" [game].\n";
          play state
      | Quit ->
          print_string "\nTHANKS FOR PLAYING :)\n";
          Stdlib.exit 0
      | Family ->
          (* print_string ("Your familial status: " ^ State.family state
             ^ "\n"); *)
          play state
      | Balance ->
          print_string
            ("Your balance: "
            ^ string_of_int (State.balance state)
            ^ "\n");
          play state
      | Prizes ->
          print_string
            ("Your prizes: "
            ^ string_list_to_string (State.prizes state) ", "
            ^ "\n");
          play state
      | Play name_of_game -> (
          let result =
            State.play state
              (string_list_to_string name_of_game " ")
              all_games
          in
          match result with
          | Illegal ->
              print_string "Illegal PLAY entry, please try again\n";
              play state
          | Legal l ->
              print_string "\n";
              play l))

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
            print_string "Illegal dollar amount, please restart game."))

let () = main ()