Random.self_init ()

type t = { winnings : int }

let winnings st = st.winnings

type bet_type =
  | Valid of int
  | Invalid

type winner =
  | AI
  | Player

let rec get_bet balance =
  try
    let bet =
      int_of_string
        (read_line
           (print_endline "How much money would you like to bet?\n"))
    in
    if balance - bet >= 0 then Valid bet
    else (
      print_endline "Insufficient Funds. Try again.";
      get_bet balance)
  with
  | Sys_error _ ->
      print_string "Illegal dollar amount (whole dollars only).\n";
      get_bet balance
  | Failure _ ->
      print_string "Illegal dollar amount (whole dollars only).\n";
      get_bet balance

let allocate_winnings winner bet_type =
  match bet_type with
  | Valid bet -> (
      match winner with
      | AI ->
          print_endline
            ("You lost " ^ string_of_int bet ^ " dollars. :(");
          { winnings = -bet }
      | Player ->
          print_endline
            ("You won " ^ string_of_int (2 * bet) ^ " dollars! :)");
          { winnings = 2 * bet })
  | Invalid -> failwith "Something's really messed up."

let rec generate_coords coords board_size ship_num =
  if ship_num = List.length coords then coords
  else
    let x = Random.int board_size in
    let y = Random.int board_size in
    let new_c = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" in
    if List.mem new_c coords then
      generate_coords coords board_size ship_num
    else generate_coords (new_c :: coords) board_size ship_num

let rec get_bs balance : int =
  try
    let bs =
      int_of_string
        (read_line (print_endline "What board size do you want?\n"))
    in
    if bs > 0 then bs
    else (
      print_endline "Invalid input. Try again.";
      get_bs balance)
  with
  | Sys_error _ ->
      print_string "Invalid input.\n";
      get_bs balance
  | Failure _ ->
      print_string "Invalid input.\n";
      get_bs balance

let rec get_num_ships bs : int =
  try
    let ns =
      read_int
        (print_endline "How many ships do you want there to be?\n")
    in
    if ns > 0 && ns <= bs * bs / 3 then ns
    else (
      print_endline "Need fewer ships. Try again.";
      get_num_ships bs)
  with
  | Sys_error _ ->
      print_string "Invalid input.\n";
      get_num_ships bs
  | Failure _ ->
      print_string "Invalid input.\n";
      get_num_ships bs

let rec user_coords coords board_size ship_num : string list =
  if ship_num = List.length coords then coords
  else (
    print_endline "Enter coordinates for new ship:\n";
    let x =
      int_of_string
        (read_line (print_endline "x coordinate of this ship\n"))
    in
    let y =
      int_of_string
        (read_line (print_endline "y coordinate of this ship\n"))
    in
    if x < board_size && y < board_size && x >= 0 && y >= 0 then
      let new_c =
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
      in
      if List.mem new_c coords then (
        print_endline "Already used these coordinates. Try again.";
        user_coords coords board_size ship_num)
      else user_coords (new_c :: coords) board_size ship_num
    else (
      print_endline "Invalid input you clown. Try again.";
      user_coords coords board_size ship_num))

let rec users_turn ai_ships user_ships board_size ships_tried =
  if List.length ai_ships = 0 then Player
  else if List.length user_ships = 0 then AI
  else
    let x =
      int_of_string (read_line (print_endline "x coordinate to hit\n"))
    in
    let y =
      int_of_string
        (read_line (print_endline "y coordinate to hit\n\n"))
    in
    if x < board_size && y < board_size && x >= 0 && y >= 0 then
      let new_c =
        "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
      in
      if List.mem new_c ai_ships then (
        print_endline "HIT!";
        ai_turn
          (List.filter (fun x -> x <> new_c) ai_ships)
          user_ships board_size ships_tried)
      else (
        print_endline "Miss.";
        ai_turn ai_ships user_ships board_size ships_tried)
    else (
      print_endline "Invalid. Try again.";
      users_turn ai_ships user_ships board_size ships_tried)

and ai_turn ai_ships user_ships board_size ships_tried =
  if List.length user_ships = 0 then AI
  else if List.length ai_ships = 0 then Player
  else
    let x = Random.int board_size in
    let y = Random.int board_size in
    let new_c = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")" in
    if List.mem new_c ships_tried then
      ai_turn ai_ships user_ships board_size ships_tried
    else if List.mem new_c user_ships then (
      print_endline "Your ship was hit!";
      users_turn ai_ships
        (List.filter (fun x -> x <> new_c) user_ships)
        board_size (new_c :: ships_tried))
    else (
      print_endline "Computer missed.";
      users_turn ai_ships user_ships board_size (new_c :: ships_tried))

let battleship balance : t =
  let bet_ty = get_bet balance in
  let bs = get_bs balance in
  let ns = get_num_ships bs in
  let ai_ships = generate_coords [] bs ns in
  let user_ships = user_coords [] bs ns in
  let winner = users_turn ai_ships user_ships bs [] in
  allocate_winnings winner bet_ty
