type t
type result

val init_state : string -> int -> t
val bank : t -> int
val family : t -> string
val play : string -> int -> t -> unit