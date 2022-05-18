val wof_commands : string list
val slots_commands : string list
val craps_commands : string list
val craps_pass_commands : string list
val craps_dontpass_commands : string list
val war_commands : string list
val bacc_commands : string list
val bk_commands : string list
val bship_commands : string list
val rps_commands : string list
val yes_commands : string list
val no_commands : string list
val help_commands : string list

val game_commands : string list
(** Above are all the commands that are valid inputs for their given
    games*)

val craps_help_string : string
val war_help_string : string

(**Above are all strings that give some help to user to understand game*)

val slot_win_amt : int
(**Amount that you can win in a slots game*)