(*Initializes randomizer for funtion [slots]*)
Random.self_init ()

type t = { winnings : int }

let winnings state = state.winnings

let slots_winnings slot1 slot2 slot3 =
  if slot1 = slot2 && slot2 = slot3 then 1000
  else if slot1 = slot2 || slot2 = slot3 || slot1 = slot3 then 500
  else 0

let slots () =
  let slot1 = Random.int 5 in
  let slot2 = Random.int 5 in
  let slot3 = Random.int 5 in
  let money = slots_winnings slot1 slot2 slot3 in
  print_endline
    ("Slot 1: " ^ string_of_int slot1 ^ "\nSlot 2: "
   ^ string_of_int slot2 ^ "\nSlot 3: " ^ string_of_int slot3
   ^ "\n\n You've won $" ^ string_of_int money);
  { winnings = money }
