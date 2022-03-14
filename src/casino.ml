(*this casino*)
open Player
open Wheeloffortune

let rec main () =
  print_endline
    "\n\n\
     Welcome to The Casino\n\
    \ What game do you want to play? (Wheel of Fortune).\n";
  print_string "> ";
  let command = read_line () in
  if command = "Wheel of Fortune" then Wheeloffortune.wheel_of_fortune
  else YouSuck
