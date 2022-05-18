type t2

type result =
  | Legal of t2
  | Illegal

type play_state =
  | Lose of string
  | Continue

val init_state : string -> int -> t2
(** Initial state of casino*)
val name : t2 -> string
(**Name of person in given state*)
val balance : t2 -> int
(**Balance of person in given state*)
val family : t2 -> string
(**Family of person in given state*)
val prizes : t2 -> string list
(**Prizes of person in given state*)
val play_st : t2 -> play_state
(**plays a game in given state*)
val play : t2 -> string -> string list -> result
(**Actually runs and goes to Game.play*)
val exchange : t2 -> string -> t2
(**Exchange items for cash*)