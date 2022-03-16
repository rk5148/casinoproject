type prize =
  | Car
  | Lose of int
  | House
  | Education
  | Nate

val prize_to_string : prize -> unit
val prize_list : prize list
val wheel_of_fortune : unit -> unit
