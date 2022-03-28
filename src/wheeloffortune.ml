open Casino
open Random

(*Initializes randomizer for funtion [wheel_of_fortune]*)

(*Defines type prize*)
type prize =
  | Car
  | Lose of int
  | House
  | Education
  | Nate

(*Matches given prize with the output string to console*)
let prize_to_string (prize : prize) =
  match prize with
  | Car -> print_endline "You've won a Car. Vroom vroom."
  | Lose n -> print_endline "You're trash at this game"
  | House -> print_endline "You've won a House. Knock knock."
  | Education -> print_endline "You've won an Education. Big woop."
  | Nate -> print_endline "You've won Nate. OOoooo."

(*All possible prizes placed in a list*)
let prize_list = [ Car; Lose 0; House; Education; Nate ]

(*Main running function to get a random prize from the list of possible
  prizes*)
let wheel_of_fortune () =
  let x = Random.int 4 in
  prize_to_string (List.nth prize_list x);
  print_endline "Would you like to spin again?";
  let input = read_line () in
  if input = "Yes" || input = "yes" then wheel_of_fortune ()
  else Casino.main
