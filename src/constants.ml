let wof_commands =
  [
    "1";
    "1 ";
    "Wheel of Fortune";
    "wheel of fortune";
    "WHEEL OF FORTUNE";
    "Wheel of Fortune ";
    "wheel of fortune ";
    "WHEEL OF FORTUNE ";
    "wof";
    "WOF";
    "wof ";
    "WOF ";
  ]

let slots_commands =
  [ "2"; "2 "; "Slots"; "slots"; "SLOTS"; "Slots "; "slots "; "SLOTS " ]

let craps_commands =
  [ "3"; "3 "; "Craps"; "craps"; "CRAPS"; "Craps "; "craps "; "CRAPS " ]

let craps_help_string =
  "Valid commands:\n" ^ "1: Pass\n" ^ "2: Don't Pass\n" ^ "help\n"

let craps_pass_commands =
  [ "1"; "1 "; "Pass"; "pass"; "PASS"; "Pass "; "PASS " ]

let craps_dontpass_commands =
  [
    "2";
    "2 ";
    "Don't Pass";
    "Dont Pass";
    "don't pass";
    "dont pass";
    "DON'T PASS";
    "DONT PASS";
    "Don't Pass ";
    "Dont Pass ";
    "don't pass ";
    "dont pass ";
    "DON'T PASS ";
    "DONT PASS ";
    "Dont pass";
    "Don't pass";
    "don't Pass";
    "dont Pass";
  ]

let war_commands =
  [ "4"; "4 "; "War"; "war"; "WAR"; "War "; "war "; "WAR " ]

let bacc_commands =
  [
    "5";
    "5 ";
    "Bacc";
    "bacc";
    "Baccarat";
    "baccarat ";
    "Baccarat ";
    "baccarat ";
    "BACCARAT";
    "BACCARAT ";
  ]

let bk_commands =
  [
    "6";
    "6 ";
    "Blackjack";
    "blackjack";
    "BLACKJACK";
    "Blackjack ";
    "blackjack ";
    "BLACKJACK ";
  ]

let bship_commands =
  [
    "7";
    "7 ";
    "Battleship";
    "battleship";
    "BATTLESHIP";
    "Battleship ";
    "battleship ";
    "BATTLESHIP ";
  ]

let rps_commands =
  [
    "8";
    "8 ";
    "Rps";
    "rps";
    "RPS";
    "Rps ";
    "rps ";
    "RPS ";
    "Rock Paper Scissor";
    "Rock Paper Scissor ";
  ]

let war_help_string = "Game of WAR!"

let yes_commands =
  [ "y"; "y "; "Y"; "Y "; "yes"; "yes "; "Yes"; "Yes "; "YES"; "YES " ]

let no_commands =
  [ "n"; "n "; "N"; "N "; "no"; "no "; "No"; "No "; "NO"; "NO " ]

let help_commands =
  [ "0"; "0 "; "Help"; "help"; "HELP"; "Help "; "help "; "HELP " ]

let game_commands =
  wof_commands @ slots_commands @ craps_commands @ war_commands
  @ bacc_commands @ bk_commands @ bship_commands @ rps_commands

let slot_win_amt = 1000
