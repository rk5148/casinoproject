(*Open all relevant files (game files)*)
open Player
open Wheeloffortune
open Slots
open Deck

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

(*Runs the game*)
let rec main () =
  print_endline
    "\n\n\
     Welcome to The Casino\n\
    \ What game do you want to play? \n\
    \ (1): Wheel of Fortune, \n\
    \ (2): Slots, \n\
    \ (3): Deal 7\n";
  print_string "> ";
  let command = read_line () in
  if List.mem command wof_commands then
    Wheeloffortune.wheel_of_fortune ()
  else if List.mem command slots_commands then Slots.slots ()
  else if List.mem command deal7_commands then Deck.play_deck ()
  else print_endline "\nInvalid input."

let _ = main ()