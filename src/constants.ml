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
  "Craps is a game of wagers between you (the shooter) and the bank \
   involving just a pair of dice! When you start, you will be asked to \
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
   established, but it can be taken down (type in '0' when asked at \
   that point to take down your bet) or reduced. \n\
  \ \n\
   Both the Pass and Don't Pass bets pay even money (e.g. you bet 100, \
   you win 100).\n\n"

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

let war_help_string = "To be written later"

let yes_commands =
  [ "y"; "y "; "Y"; "Y "; "yes"; "yes "; "Yes"; "Yes "; "YES"; "YES " ]

let no_commands =
  [ "n"; "n "; "N"; "N "; "no"; "no "; "No"; "No "; "NO"; "NO " ]

let help_commands =
  [ "0"; "0 "; "Help"; "help"; "HELP"; "Help "; "help "; "HELP " ]

let game_commands =
  wof_commands @ slots_commands @ craps_commands @ war_commands
  @ bacc_commands @ bk_commands @ bship_commands @ rps_commands
