Random.self_init ()

open Constants

type t = { winnings : int }

let winnings state = state.winnings
let help_string = Constants.craps_help_string

let roll_two_dice num =
  print_endline "\nShoot away!";
  let roll1 = 1 + Random.int num in
  let roll2 = 1 + Random.int num in
  let number = roll1 + roll2 in
  print_endline (string_of_int roll1);
  print_endline (string_of_int roll2);
  print_endline ("You've shot " ^ string_of_int number ^ ".");
  number

let rec craps_point bet is_pass hit_point point =
  let number = roll_two_dice 6 in
  if number = 7 && (not hit_point) && is_pass then begin
    print_endline "Seven-out! \nYou've lost.";
    { winnings = -1 * bet }
  end
  else if number = 7 && (not hit_point) && not is_pass then begin
    print_endline
      ("Seven-out! You've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.");
    { winnings = 2 * bet }
  end
  else if number = 7 && hit_point && is_pass then begin
    print_endline "Seven-out! \nYou've lost.";
    { winnings = -1 * bet }
  end
  else if number = 7 && hit_point && not is_pass then begin
    print_endline
      ("Seven-out! \nYou've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.");
    { winnings = 2 * bet }
  end
  else if number = point && is_pass then begin
    print_endline
      ("Point hits! \nYou've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.");
    { winnings = 2 * bet }
  end
  else if number = point && not is_pass then begin
    print_endline "Point hits! \nYou've lost.";
    { winnings = -1 * bet }
  end
  else craps_point bet is_pass hit_point point

let rec comeout_notpass_bet bet is_pass point =
  print_string "Place new bet: ";
  print_string "\n> ";
  match int_of_string_opt (read_line ()) with
  | Some int ->
      if int <= bet && int > 0 then craps_point int is_pass false point
      else if bet = 0 then begin
        print_endline "Thanks for playing!";
        { winnings = 0 }
      end
      else begin
        print_endline
          "You must bet an amount greater than or equal to your \
           original bet.";
        comeout_notpass_bet bet is_pass point
      end
  | None ->
      print_endline
        "Please type in a valid bet less than or equal to your \
         original bet, or type in '0' to take down your bet. We only \
         accept Casino Cash.";
      comeout_notpass_bet bet is_pass point

let rec comeout_pass_bet bet is_pass point =
  print_string "\nPlace new bet: ";
  print_string "\n> ";
  match int_of_string_opt (read_line ()) with
  | Some int ->
      if int >= bet then craps_point int is_pass false point
      else begin
        print_endline
          "You must bet an amount greater than or equal to your \
           original bet.";
        comeout_pass_bet bet is_pass point
      end
  | None ->
      print_endline
        "Please type in a valid bet greater than or equal to your \
         original bet. We only accept Casino Cash.";
      comeout_pass_bet bet is_pass point

let rec comeout_bet bet is_pass point =
  if is_pass then begin
    print_endline "Would you like to increase your bet?";
    print_string "\n> ";
    let command = read_line () in
    if command = "yes" || command = "Yes" then
      comeout_pass_bet bet is_pass point
    else if command = "no" || command = "No" then
      craps_point bet is_pass false point
    else begin
      print_endline
        "I didn't understand you. Type 'yes' or 'Yes' to make a new \
         bet, or type 'no' or 'No' to continue.";
      comeout_bet bet is_pass point
    end
  end
  else begin
    print_endline "Would you like to reduce or take down your bet?";
    print_string "\n> ";
    let command = read_line () in
    if command = "yes" || command = "Yes" then
      comeout_notpass_bet bet is_pass point
    else if command = "no" || command = "No" then
      craps_point bet is_pass false point
    else begin
      print_endline
        "I didn't understand you. Type 'yes' or 'Yes' to make a new \
         bet, or type 'no' or 'No' to continue.";
      comeout_bet bet is_pass point
    end
  end

let craps_come_out bet is_pass =
  let number = roll_two_dice 6 in
  if (number = 2 || number = 3 || number = 12) && is_pass then begin
    print_endline (string_of_int number ^ " craps! \nYou've lost.");
    { winnings = -1 * bet }
  end
  else if (number = 2 || number = 3 || number = 12) && not is_pass then begin
    print_endline
      (string_of_int number ^ " craps! \nYou've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.");
    { winnings = 2 * bet }
  end
  else if number = 12 && not is_pass then begin
    print_endline (string_of_int number ^ " craps! \nIt's a push!");
    { winnings = 0 }
  end
  else if (number = 7 || number = 11) && is_pass then begin
    print_endline
      (string_of_int number ^ " natural! \nYou've won "
      ^ string_of_int (2 * bet)
      ^ " Casino Cash.");
    { winnings = 2 * bet }
  end
  else if (number = 7 || number = 11) && not is_pass then begin
    print_endline (string_of_int number ^ " natural! \nYou've lost.");
    { winnings = -1 * bet }
  end
  else begin
    print_endline ("Point established: " ^ string_of_int number ^ ".\n");
    comeout_bet bet is_pass number
  end

let is_pass command = List.mem command Constants.craps_pass_commands

let is_dont_pass command =
  List.mem command Constants.craps_dontpass_commands

let is_help command = List.mem command Constants.help_commands

let rec craps bet =
  print_endline "\nPlace your bets:";
  print_endline "(1): Pass";
  print_endline "(2): Don't Pass";
  print_string "\n> ";
  let command = read_line () in
  if is_pass command then craps_come_out bet true
  else if is_dont_pass command then craps_come_out bet false
  else if is_help command then begin
    print_string help_string;
    craps bet
  end
  else begin
    print_string
      "Invalid bet. To bet on the Pass line, type 'Pass'. To bet on \
       Don't Pass, type 'Don't Pass'. \n\
       If you would like help, type 'Help'.\n";
    craps bet
  end
