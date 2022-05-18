type t

val winnings : t -> int

val slots_winnings : int -> int -> int -> int
(** [slots_winnings slot1 slot2 slot3] determines the winnings from the
    given slot reel values. If all three slots match, then winnings are
    big. If only two of the three match, winnings are not as big. If
    none match, then winnings are 0. *)

val slots : unit -> t
(** [slots ()] pulls of lever on a slot machine. *)