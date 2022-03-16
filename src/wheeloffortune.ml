(*wheel of fort*)

type prize =
  | Car
  | Lose of int
  | House
  | Education
  | Nate
  | YouSuck

let prize_to_string (prize : prize) =
  match prize with
  | Car -> print_endline "Thank you for playing with us!!!!!!!1"
  | Lose n -> print_endline "Thank you for playing with us!!!!!!!2"
  | House -> print_endline "Thank you for playing with us!!!!!!!3"
  | Education -> print_endline "Thank you for playing with us!!!!!!!4"
  | Nate -> print_endline "Thank you for playing with us!!!!!!!5"
  | YouSuck -> print_endline "Thank you for playing with us!!!!!!!6"

let prize_list = [ Car; Lose 0; House; Education; Nate; YouSuck ]

let wheel_of_fortune () =
  let x = Random.int 4 in
  let _ = print_endline "Thank you for playing with us!!!!!!!" in
  prize_to_string (List.nth prize_list x)
