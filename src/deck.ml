type suit =
  | Spade
  | Diamond
  | Heart
  | Clubs

type card = {
  suit : suit;
  value : int;
}

let a_heart = { suit = Heart; value = 14 }

let rec make_hearts last_val =
  if last_val + 1 = 14 then { suit = Heart; value = 14 }
