open Wheeloffortune
open Slots
open Constants

exception GameNotFound

type t = {
  name : string;
  balance : int;
  prize_list : string list;
}

let name gt = gt.name
let balance gt = gt.balance
let prizes gt = gt.prize_list

let woft_to_gt wof_t =
  {
    name = "";
    balance = 0;
    prize_list = [ Wheeloffortune.prize wof_t ];
  }

let slotst_to_gt slotst =
  { name = ""; balance = Slots.winnings slotst; prize_list = [] }

let is_wof str = List.mem str Constants.wof_commands
let is_slots str = List.mem str Constants.slots_commands

let play (name_of_game : string) =
  if is_wof name_of_game then
    woft_to_gt (Wheeloffortune.wheel_of_fortune ())
  else if is_slots name_of_game then slotst_to_gt (Slots.slots ())
  else raise GameNotFound
