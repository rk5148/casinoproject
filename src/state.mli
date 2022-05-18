type t2

type result =
  | Legal of t2
  | Illegal

type play_state =
  | Lose of string
  | Continue

val init_state : string -> int -> t2
val name : t2 -> string
val balance : t2 -> int
val prizes : t2 -> string list
val play_st : t2 -> play_state
val play : t2 -> string -> string list -> result
val exchange : t2 -> string -> t2