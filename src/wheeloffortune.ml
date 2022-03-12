(*wheel of fort*)
open Random
open Person

let wheeloffortune money =
  let possibilities_list =
    [ Car; Lose money; House; Education; Nate ]
  in
  let x = Random.int 4 in
  let _ = print_endline "Thank you for playing with us!!!!!!!" in
  List.nth possibilities_list x
