open Random

let help_string =
  "Craps is a game of wagers between you (the shooter) and the bank \
   involving just a pair dice! When you start, you will be asked to \
   place bets on either the Pass line (the bet for the shooter to win) \
   or Don't Pass (the bet for the shooter to lose). \n\
  \ \n\
  \ Then, there are two phrases to Craps: come-out and point. The \
   first roll is known as come-out. A come-out roll of 2, 3, or 12 is \
   called 'craps', 7 or 11 is a 'natural', and any other number (4, 5, \
   6, 8, 9, 10) establishes the point for the point phase. In order \
   for the shooter (you) to pass, you must roll this point number \
   again before rolling a seven. \n\
  \ \n\
  \ Here are the possibilities for a Pass line bet: \n\
  \ Come-out roll of 7 or 11 means the bet wins. \n\
   Come-out roll of 2, 3, or 12 means the bet loses. \n\
   Come-out roll of any other number n establishes a point: if n is \
   rolled again before a 7, the bet wins; if a 7 rolled is rolled \
   before n is rolled again ('seven out'), the bet loses. \n\
  \ \n\
   Here are the possibilities for a Don't Pass bet: \n\
  \ Come-out roll of 7 or 11 means the bet loses. \n\
   Come-out roll of 2 or 3 means the bet wins. \n\
  \ Come-out roll of 12 means the bet is a push (neither a win or a \
   loss). \n\
   Come-out roll of any other number n establishes a point: if n is \
   rolled again before a 7, the bet loses; if a 7 rolled is rolled \
   before n is rolled again ('seven out'), the bet wins. \n\
  \ \n\
   A Pass line bet, once made, cannot be taken down or reduced until \
   the round is over. However, it can be increased after a point is \
   established. \n\
   A Don't Pass bet, once made, cannot be increased after a point is \
   established, but it can be taken down or reduced. \n\
  \ \n\
   Both the Pass and Don't Pass bets pay even money (e.g. you bet 100, \
   you win 100)."

let roll_two_dice num =
  print_endline "Shoot away!";
  let roll1 = 1 + Random.int num in
  let roll2 = 1 + Random.int num in
  let number = roll1 + roll2 in
  print_endline ("You've shot " ^ string_of_int number ^ ".");
  number

let rec craps_point bet is_pass hit_point point =
  let number = roll_two_dice 5 in
  if number = 7 && (not hit_point) && is_pass then
    print_endline "Seven-out! You've lost."
  else if number = 7 && (not hit_point) && not is_pass then
    print_endline
      ("Seven-out! You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.")
  else if number = 7 && hit_point && is_pass then
    print_endline "Seven-out! You've lost."
  else if number = 7 && hit_point && not is_pass then
    print_endline
      ("Seven-out! You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.")
  else if number = point && is_pass then
    print_endline
      ("Point hits! You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.")
  else if number = point && not is_pass then
    print_endline "Point hits! You've lost."
  else craps_point bet is_pass hit_point point

let rec comeout_notpass_bet bet is_pass point =
  print_string "Place new bet: ";
  match int_of_string_opt (read_line ()) with
  | Some int ->
      if int <= bet && int > 0 then craps_point int is_pass false point
      else if bet = 0 then print_endline "Thanks for playing!"
      else
        print_endline
          "You must bet an amount greater than or equal to your \
           original bet.";
      comeout_notpass_bet bet is_pass point
  | None ->
      print_endline
        "Please type in a valid bet less than or equal to your \
         original bet, or type in '0' to take down your bet. We only \
         accept Casino Cash.";
      comeout_notpass_bet bet is_pass point

let rec comeout_pass_bet bet is_pass point =
  print_string "Place new bet: ";
  match int_of_string_opt (read_line ()) with
  | Some int ->
      if int >= bet then craps_point int is_pass false point
      else
        print_endline
          "You must bet an amount greater than or equal to your \
           original bet.";
      comeout_pass_bet bet is_pass point
  | None ->
      print_endline
        "Please type in a valid bet greater than or equal to your \
         original bet. We only accept Casino Cash.";
      comeout_pass_bet bet is_pass point

let rec comeout_bet bet is_pass point =
  if is_pass then begin
    print_endline "Would you like to increase your bet?";
    let command = read_line () in
    if command = "yes" || command = "Yes" then
      comeout_pass_bet bet is_pass point
    else if command = "no" || command = "No" then
      craps_point bet is_pass false point
    else
      print_endline
        "I didn't understand you. Type 'yes' or 'Yes' to make a new \
         bet, or type 'no' or 'No' to continue.";
    comeout_bet bet is_pass point
  end
  else begin
    print_endline "Would you like to reduce or take down your bet?";
    let command = read_line () in
    if command = "yes" || command = "Yes" then
      comeout_notpass_bet bet is_pass point
    else if command = "no" || command = "No" then
      craps_point bet is_pass false point
    else
      print_endline
        "I didn't understand you. Type 'yes' or 'Yes' to make a new \
         bet, or type 'no' or 'No' to continue.";
    comeout_bet bet is_pass point
  end

let craps_come_out bet is_pass =
  let number = roll_two_dice 5 in
  if (number = 2 || number = 3 || number = 12) && is_pass then
    print_endline (string_of_int number ^ " craps! \n You've lost.")
  else if (number = 2 || number = 3 || number = 12) && not is_pass then
    print_endline
      (string_of_int number ^ " craps! \n You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.")
  else if number = 12 && not is_pass then
    print_endline (string_of_int number ^ " craps! \n It's a push!")
  else if (number = 7 || number = 11) && is_pass then
    print_endline
      (string_of_int number ^ " natural! \n You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.")
  else if (number = 7 || number = 11) && not is_pass then
    print_endline (string_of_int number ^ " natural! \n You've lost.")
  else print_endline ("Point established: " ^ string_of_int number ^ ".");
  comeout_bet bet is_pass number

let rec craps_start bet =
  print_endline "Place your bets:";
  print_endline "Pass";
  print_endline "Don't Pass";
  let command = read_line () in
  if command = "Pass" then craps_come_out bet true
  else if command = "Don't Pass" then craps_come_out bet false
  else if command = "Help" then begin
    print_string help_string;
    craps_start bet
  end
  else
    print_string
      "Invalid bet. To bet on the Pass line, type 'Pass'. To bet on \
       Don't Pass, type 'Don't Pass'. \n\
      \ If you would like help, type 'Help'.";
  craps_start bet
