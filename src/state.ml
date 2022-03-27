open Command

type t = {
  bank : int;
  name : string;
  family_status : string;
}

type result =
  | Legal of t
  | Illegal

type win_cond =
  | Lose of string
  | Continue

let init_state name start_money =
  { name; bank = start_money; family_status = "Lonely" }

let bank st = st.bank
let family st = st.family_status

let rec string_list_to_string lst =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | h :: t -> h ^ "\n" ^ string_list_to_string t

let rec print_list = function
  | [] -> ""
  | h :: t ->
      print_string (h ^ "\n");
      print_list t

let string_to_string_list str =
  str |> String.split_on_char ' ' |> List.filter (fun x -> x <> "")

let contains search target = List.mem target search

let string_list_to_string lst =
  match lst with
  | [] -> ""
  | [ x ] -> x
  | h :: t -> h ^ ", " ^ string_list_to_string t

let play (game : string) (bet : int) (st : t) = print_string ""
(* if game = let all_exits = try Some (Adventure.exits adv
   st.current_room) with UnknownRoom _ -> None in match all_exits with |
   None -> Illegal | Some exit -> if List.mem ex exit then let
   room_to_move_to = Adventure.next_room adv st.current_room ex in let
   visited_rooms = List.sort_uniq compare (room_to_move_to ::
   st.visited) in Legal { current_room = room_to_move_to; visited =
   visited_rooms; inventory = st.inventory; not_picked_up =
   st.not_picked_up; score = (if List.mem room_to_move_to st.visited
   then st.score else st.score + Adventure.points adv room_to_move_to);
   already_got_points = st.already_got_points; } else Illegal *)
