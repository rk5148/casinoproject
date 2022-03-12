type prize

module type Person = sig
  val prizes : prize list
  val current_money : int
  val familial_status : string
  val name : string
end

val current_prizes : prize list
val current_money : int
val current_family : string
val current_name : string
