(** Defines type prize *)
type prize =
  | Car
  | Lose of int
  | House
  | Education
  | Nate

val prize_to_string : prize -> unit
(** Matches given prize with the output string to console *)

val prize_list : prize list
(** All possible prizes placed in a list *)

val wheel_of_fortune : unit -> unit
(** Main running function to get a random prize from the list of
    possible prizes *)
