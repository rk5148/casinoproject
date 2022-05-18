type t

val winnings : t -> int
(** Returns winnings of current state*)

val war : int -> t
(** Simulates a round of war, returns any winnings *)