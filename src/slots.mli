type t

val winnings : t -> int

val slots_winnings : 'a -> 'a -> 'a -> int
(** Determines the winnings from the given slot reel values *)

val slots : unit -> t
(** Simulates pull of lever on a slot machine *)