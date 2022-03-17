(*Initializes randomizer for funtion [slots]*)
Random.self_init ()

(*Determines the winnings from the given slot reel values*)
let slots_winnings slot1 slot2 slot3 =
  if slot1 = slot2 && slot2 = slot3 then 1000
  else if slot1 = slot2 || slot2 = slot3 || slot1 = slot3 then 500
  else 0

(*Simulates pull of lever on a slot machine*)
let slots () =
  let slot1 = Random.int 5 in
  let slot2 = Random.int 5 in
  let slot3 = Random.int 5 in
  print_endline
    ("Slot 1: " ^ string_of_int slot1 ^ "\n Slot 2: "
   ^ string_of_int slot2 ^ "\n Slot 3: " ^ string_of_int slot3
   ^ "\n \n You've won $"
    ^ string_of_int (slots_winnings slot1 slot2 slot3))