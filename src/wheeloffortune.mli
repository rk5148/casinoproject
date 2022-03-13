module type WheelOfFortune = sig
  type prize

  val prize_list : prize list
  val wheel_of_fortune : prize
end