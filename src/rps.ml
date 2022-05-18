Random.self_init ()

type t = { winnings : int }

let winnings st = st.winnings

type bet_type =
  | Valid of int
  | Invalid

type winner =
  | AI
  | Player

type move =
  | Rock
  | Paper
  | Scissor

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

let rec user_move balance =
  print_endline "Your move (Rock-Paper-Scissor):\n";
  let move = read_line () in
  match move with
  | "Rock" -> Rock
  | "Paper" -> Paper
  | "Scissor" -> Scissor
  | _ ->
      print_endline "Invalid input. Try again.";
      user_move balance

let rec ai_move balance =
  let x = Random.int 3 in
  match x with
  | 0 -> Rock
  | 1 -> Paper
  | 2 -> Scissor
  | _ -> ai_move balance

let rec moves balance =
  let user_m = user_move balance in
  let ai_m = ai_move balance in
  logic user_m ai_m balance

and logic user_move ai_move balance =
  match (user_move, ai_move) with
  | Rock, Rock ->
      print_endline "AI played: Rock\n";
      moves balance
  | Rock, Paper ->
      print_endline "AI played: Paper\n";
      AI
  | Rock, Scissor ->
      print_endline "AI played: Scissor\n";
      Player
  | Paper, Paper ->
      print_endline "AI played: Paper\n";
      moves balance
  | Paper, Rock ->
      print_endline "AI played: Rock\n";
      Player
  | Paper, Scissor ->
      print_endline "AI played: Scissor\n";
      AI
  | Scissor, Scissor ->
      print_endline "AI played: Scissor\n";
      moves balance
  | Scissor, Rock ->
      print_endline "AI played: Rock\n";
      AI
  | Scissor, Paper ->
      print_endline "AI played: Paper\n";
      Player

let rps balance : t =
  let bet_ty = get_bet balance in
  let winner = moves balance in
  allocate_winnings winner bet_ty
