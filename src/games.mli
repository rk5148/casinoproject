exception GameNotFound

type t

val name : t -> string
val balance : t -> int
val prizes : t -> string list
val play : string -> int -> t
