Random.self_init ()

let craps_play bet is_pass =
  print_endline "Shoot away!";
  let roll1 = 1 + Random.int 5 in
  let roll2 = 1 + Random.int 5 in
  print_endline (string_of_int roll1);
  print_endline (string_of_int roll2);
  let number = roll1 + roll2 in
  print_endline ("You've shot " ^ string_of_int number ^ ".");
  if (number = 2 || number = 3 || number = 12) && is_pass then
    print_endline (string_of_int number ^ " craps! \n You've lost.")
  else if (number = 2 || number = 3 || number = 12) && not is_pass then
    print_endline
      (string_of_int number ^ " craps! \n You've won "
      ^ string_of_int (2 * bet)
      ^ ".")
  else if (number = 7 || number = 11) && is_pass then
    print_endline
      (string_of_int number ^ " natural! \n You've won "
      ^ string_of_int (2 * bet)
      ^ ".")
  else if (number = 7 || number = 11) && not is_pass then
    print_endline (string_of_int number ^ " natural! \n You've lost.")

let rec craps_bet bet =
  print_endline "Place your bets:";
  print_endline "Pass";
  print_endline "Don't Pass";
  let command = read_line () in
  if command = "Pass" then craps_play bet true
  else if command = "Don't Pass" then craps_play bet false
  else
    print_endline
      "Invalid bet. To bet on the Pass line, type 'Pass'. To bet on \
       the Don't Pass line, type 'Don't Pass'.";
  craps_bet bet
