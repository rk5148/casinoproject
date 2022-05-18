exception GameNotFound

type t

val name : t -> string
(**Returns the name of the person in a given state st*)

val balance : t -> int
(**Returns the name of the balance in a given state st*)

val prizes : t -> string list
(**Returns the name of the prizes in a given state st*)

val play : string -> int -> t
(**plays a specified game with a specified balance*)
