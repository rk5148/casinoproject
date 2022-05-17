open Wheeloffortune
open Slots
open Craps
open War
open Baccarat
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

let crapst_to_gt crapst =
  { name = ""; balance = Craps.winnings crapst; prize_list = [] }

let warst_to_gt wart =
  { name = ""; balance = War.winnings wart; prize_list = [] }

let bacct_to_gt bacct =
  { name = ""; balance = Baccarat.winnings bacct; prize_list = [] }

let is_wof str = List.mem str Constants.wof_commands
let is_slots str = List.mem str Constants.slots_commands
let is_craps str = List.mem str Constants.craps_commands
let is_war str = List.mem str Constants.war_commands
let is_bacc str = List.mem str Constants.bacc_commands

let play (name_of_game : string) (balance : int) =
  if is_wof name_of_game then
    woft_to_gt (Wheeloffortune.wheel_of_fortune ())
  else if is_slots name_of_game then slotst_to_gt (Slots.slots ())
  else if is_craps name_of_game then crapst_to_gt (Craps.craps balance)
  else if is_war name_of_game then warst_to_gt (War.war balance)
  else if is_bacc name_of_game then
    bacct_to_gt (Baccarat.baccarat balance)
  else raise GameNotFound
