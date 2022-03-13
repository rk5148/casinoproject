(*wheel of fort*)
module WheelOfFortune = struct
  type prize =
    | Car
    | Lose of int
    | House
    | Education
    | Nate

  let prize_list = [ Car; Lose 0; House; Education; Nate ]

  let wheel_of_fortune money =
    let x = Random.int 4 in
    let _ = print_endline "Thank you for playing with us!!!!!!!" in
    List.nth prize_list x
end