Random.self_init ()

(*Initializes randomizer for funtion [wheel_of_fortune]*)
type t = { prize : string }

(*Defines type prize*)
type prize =
  | Car
  | Lose of int
  | House
  | Education

let prize state = state.prize

(*Matches given prize with the output string to console*)
let prize_to_string (prize : prize) =
  match prize with
  | Car -> print_endline "You've won a Car. Vroom vroom."
  | Lose n -> print_endline "You're trash at this game"
  | House -> print_endline "You've won a House. Knock knock."
  | Education -> print_endline "You've won an Education. Big woop."

let prize_id (prize : prize) =
  match prize with
  | Car -> "Car"
  | Lose n -> ""
  | House -> "House"
  | Education -> "Education"

(*All possible prizes placed in a list*)
let prize_list = [ Car; Lose 0; House; Education ]

(*Main running function to get a random prize from the list of possible
  prizes*)
let wheel_of_fortune () =
  let x = Random.int 4 in
  prize_to_string (List.nth prize_list x);
  { prize = prize_id (List.nth prize_list x) }
