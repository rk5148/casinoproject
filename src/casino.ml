(*Open all relevant files (game files)*)
open Player
open Wheeloffortune
open Slots
open Deck

(*Runs the game*)
let rec main () =
  print_endline
    "\n\n\
     Welcome to The Casino\n\
    \ What game do you want to play? (Wheel of Fortune), (Slots), \
     (Deal 7).\n";
  print_string "> ";
  let command = read_line () in
  if command = "Wheel of Fortune" then
    Wheeloffortune.wheel_of_fortune ()
  else if command = "Slots" then Slots.slots ()
  else if command = "Deal 7" then Deck.play_deck ()
  else print_endline "\nInvalid input, stupid."

let _ = main ()