val roll_two_dice : int -> int
(** [roll_two_dice num] is the sum of rolling two ([num]+1)-sided dice
    and prints out each roll as well as the sum. Requires: [num] is
    greater than 0.*)

val craps_point : int -> bool -> bool -> bool -> int -> unit
(** [craps_point bet is_pass hit_point point] is the text interface for
    the point phase of Craps. The result is determined by [hit_seven],
    [is_pass], [hit_point], and [point] based on the rules for the point
    phase of (casino) Craps. Requires: bet is greater than 0 and less
    than or equal to the total amount of money the player has.*)

val comeout_notpass_bet : int -> bool -> unit
(** [comeout_pass_bet bet is_pass point] is the text interface for the
    actual transition betting after the come-out phase and before the
    point phase if the player says yes and has already bet on Don't Pass
    (as determined by [is_pass]). Requires: [bet] is greater than 0 and
    less than the total amount of money the player has.*)

val comeout_pass_bet : int -> bool -> unit
(** [comeout_pass_bet bet is_pass point] is the text interface for the
    actual transition betting after the come-out phase and before the
    point phase if the player says yes and has already bet on the Pass
    line (as determined by [is_pass]). Requires: [bet] is greater than 0
    and less than or equal to the total amount of money the player has.*)

val comeout_bet : int -> bool -> unit
(** [comeout_bet bet is_pass point] is the text interface for asking the
    player their choice on whether to bet in the transition betting
    after the come-out phase and before the point phase. The choice of
    transition betting depends on [is_pass]. Requires: [bet] is greater
    than 0 and less than or equal to the total amount of money the
    player has.*)

val craps_come_out : int -> bool -> unit
(** [craps_come_out bet is_pass] is the text interface for the come-out
    phase of Craps, which based on certain rolls and [is_pass] will
    result in a loss of [bet], a win of 2*[bet], or the transition into
    the point phase. Requires: [bet] is greater than 0 and less than or
    equal to the total amount of money the player has.*)

val craps_start : int -> unit
(** [craps_start bet] is the text interface for the beginning of Craps,
    which asks for the user to bet [bet] on either the Pass line or
    Don't Pass, or to ask for Help. Requires: [bet] is greater than 0
    and less than or equal to the total amount of money the player has.*)
