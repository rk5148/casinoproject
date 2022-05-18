Random.self_init ()
(*Initializes randomizer for funtion [slots]*)

open Constants

type t = { winnings : int }

type bet =
  | Valid of int
  | Invalid

let winnings state = state.winnings

let slots_winnings slot1 slot2 slot3 =
  if slot1 = slot2 && slot2 = slot3 then slot_win_amt else 0

let total_winnings slot1 slot2 slot3 slot4 slot5 slot6 slot7 slot8 slot9
    =
  slots_winnings slot1 slot2 slot3
  + slots_winnings slot1 slot4 slot7
  + slots_winnings slot1 slot5 slot9
  + slots_winnings slot2 slot5 slot8
  + slots_winnings slot3 slot5 slot7
  + slots_winnings slot3 slot6 slot9
  + slots_winnings slot4 slot5 slot6
  + slots_winnings slot7 slot8 slot9

let slots () =
  let slot1 = Random.int 11 in
  let slot2 = Random.int 11 in
  let slot3 = Random.int 11 in
  let slot4 = Random.int 11 in
  let slot5 = Random.int 11 in
  let slot6 = Random.int 11 in
  let slot7 = Random.int 11 in
  let slot8 = Random.int 11 in
  let slot9 = Random.int 11 in
  let money =
    total_winnings slot1 slot2 slot3 slot4 slot5 slot6 slot7 slot8 slot9
  in
  print_endline
    ("Slot 1: " ^ string_of_int slot1 ^ "   Slot 2: "
   ^ string_of_int slot2 ^ "   Slot 3: " ^ string_of_int slot3
   ^ "\nSlot 4: " ^ string_of_int slot4 ^ "  Slot 5: "
   ^ string_of_int slot5 ^ "  Slot 6: " ^ string_of_int slot6
   ^ "\nSlot 7: " ^ string_of_int slot7 ^ "   Slot 8: "
   ^ string_of_int slot8 ^ "   Slot 9: " ^ string_of_int slot9
   ^ "\n\n You've won $" ^ string_of_int money);
  { winnings = money }
