open Games
open Constants

type t2 = {
  name : string;
  balance : int;
  family_status : string;
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
    family_status = "Lonely";
    prize_list = [];
  }

let name st = st.name
let balance st = st.balance
let family st = st.family_status
let prizes st = st.prize_list
let contains search target = List.mem target search

let play_legal_helper state name_of_game all_games =
  balance state > 0 && contains Constants.game_commands name_of_game

let gt_to_st (gt : Games.t) =
  {
    name = Games.name gt;
    balance = Games.balance gt;
    family_status = "Lonely";
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
          family_status = "Lonely";
          prize_list = prizes cleaned_up @ prizes state;
        }
      in
      Legal new_state

let remove_prize (st : t2) (prize : string) =
  List.filter (fun x -> x <> prize) (prizes st)

let rec exchange (st : t2) (prize : string) =
  match prize with
  | "Car" ->
      if List.mem "Car" (prizes st) then (
        print_endline "Exchanging Car for $1000";
        {
          name = name st;
          balance = balance st + 1000;
          family_status = "Lonely";
          prize_list = remove_prize st prize;
        })
      else exchange st ""
  | "Education" ->
      if List.mem "Education" (prizes st) then (
        print_endline "Exchanging Education for $50000";
        {
          name = name st;
          balance = balance st + 50000;
          family_status = "Lonely";
          prize_list = remove_prize st prize;
        })
      else exchange st ""
  | "House" ->
      if List.mem "House" (prizes st) then (
        print_endline "Exchanging House for $10000";
        {
          name = name st;
          balance = balance st + 10000;
          family_status = "Lonely";
          prize_list = remove_prize st prize;
        })
      else exchange st ""
  | _ ->
      print_endline "Invalid input, no exchange occurred.";
      st
