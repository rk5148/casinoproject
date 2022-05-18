type t

val winnings : t -> int

val slots_winnings : int -> int -> int -> int
(** [slots_winnings slot1 slot2 slot3] determines the winnings from the
    given 3 slot reel values in a row. If all three slots match, then
    winnings are big. If none match, then winnings are 0. Requires:
    [slot1], [slot2], [slot3] represent a three-in-a-row (row, column,
    diagonal) in a 3x3 grid.*)

val total_winnings :
  int -> int -> int -> int -> int -> int -> int -> int -> int -> int
(** [slots_winnings slot1 slot2 slot3 slot4 slot5 slot6 slot7 slot8 slot9]
    determines the total winnings from the slot machine. A slot machine
    is represented as a 3x3 matrix:
    [\[ slot1 slot2 slot3 \];
    \[ slot4 slot5 slot6 \];
    \[ slot7 slot8 slot9 \]].
    \ Winnings are earned if any row, column, or diagonal is identical
    with every spin. The total winnings are the sum of all such
    possibilities. Requires: [slot1], [slot2], [slot3], [slot4],
    [slot5], [slot6], [slot7], [slot8], [slot9] represent a a 3x3 grid.*)

val slots : unit -> t
(** [slots ()] pulls of lever on a slot machine. *)