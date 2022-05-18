(* open Constants

   type t = { winnings : int }

   let winnings st = st.winnings

   let prize_to_string (prize : prize) = match prize with | Car -> "Car"
   | House -> "House" | Education -> "Education"

   let rec items_to_string prizes = match prizes with | [] -> "" | [ x ]
   -> prize_to_string x ^ "\n" | h :: t -> prize_to_string h ^ "\n" ^
   items_to_string t

   let rec items_to_string_list prizes = match prizes with | [] -> [] |
   [ x ] -> prize_to_string x | h :: t -> prize_to_string h ::
   items_to_string_list t

   let rec choose prizes = print_endline "What item would you like to
   exchange?"; print_endline "These are your prizes:"; print_endline
   (items_to_string prizes); let chosen = read_line () in let in_list =
   List.mem chosen (items_to_string_list prizes) in match in_list with |
   true -> chosen | false -> print_endline "Not in the list, try
   again."; choose prizes

   let exchange balance prizes = match choose prizes with | "Car" -> {
   winnings = balance + 5000 } | "House" -> { winnings = balance + 10000
   } | "Education" -> { winnings = balance + 4 } | _ -> { winnings =
   balance } *)
