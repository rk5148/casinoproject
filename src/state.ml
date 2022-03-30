(* open Command *)
open Games
open Constants

type t2 = {
  name : string;
  balance : int;
  (* family_status : string; *)
  prize_list : string list;
}

type result =
  | Legal of t2
  | Illegal

type play_state =
  | Lose of string
  | Continue

let init_state name start_money =
  {
    name;
    balance = start_money;
    (* family_status = "Lonely"; *)
    prize_list = [];
  }

let name st = st.name
let balance st = st.balance

(* let family st = st.family_status *)
let prizes st = st.prize_list

(* let rec string_list_to_string lst = match lst with | [] -> "" | [ x ]
   -> x | h :: t -> h ^ "\n" ^ string_list_to_string t *)

(* let rec print_list = function | [] -> "" | h :: t -> print_string (h
   ^ "\n"); print_list t *)

(* let string_to_string_list str = str |> String.split_on_char ' ' |>
   List.filter (fun x -> x <> "") *)

let contains search target = List.mem target search

(* let string_list_to_string lst = match lst with | [] -> "" | [ x ] ->
   x | h :: t -> h ^ ", " ^ string_list_to_string t *)
let play_legal_helper state name_of_game all_games =
  balance state > 0
  && contains
       (Constants.wof_commands @ Constants.slots_commands
      @ Constants.craps_commands)
       name_of_game

let gt_to_st (gt : Games.t) =
  {
    name = Games.name gt;
    balance = Games.balance gt;
    prize_list = Games.prizes gt;
  }

let play_st st =
  if balance st = 0 then Lose "You're out of money, you lose. :(\n"
  else Continue

let play state (name_of_game : string) (all_games : string list) =
  let old_balance = balance state in
  match play_legal_helper state name_of_game all_games with
  | false -> Illegal
  | true ->
      let game_t = Games.play name_of_game old_balance in
      let cleaned_up = gt_to_st game_t in
      let new_state =
        {
          name = name state ^ name cleaned_up;
          balance = balance cleaned_up + balance state;
          prize_list = prizes cleaned_up @ prizes state;
        }
      in
      Legal new_state
