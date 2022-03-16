(*this casino*)
open Player
open Wheeloffortune
open Slots

let rec main () =
  print_endline
    "\n\n\
     Welcome to The Casino\n\
    \ What game do you want to play? (Wheel of Fortune), (Slots).\n";
  print_string "> ";
  let command = read_line () in
  if command = "Wheel of Fortune" then
    Wheeloffortune.wheel_of_fortune ()
  else if command = "Slots" then Slots.slots ()
  else print_endline "\nInvalid input, stupid."

let _ = main ()