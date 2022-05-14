(** Type suit of the possible suits any given card can have*)
type suit =
  | Spades
  | Diamonds
  | Hearts
  | Clubs

type card = {
  suit : suit;
  value : int;
}
(** Declares type of a card, requring a valid [suit] and valid [value]*)
val value_of_card : card -> int

val make_card : suit -> int -> card
(** Creates a card with given [suit] and [value]*)

val make_suit : suit -> int -> card list
(** Fills the given suit from card value 2 to value 14 (2 to Ace)*)

val make_deck : card list
(** Fills the deck with cards of all suits from 2 of Spades to Ace of
    Clubs*)

val remove_card : card list -> card -> card list
(** A copy of a deck with a given card removed.*)

val pull_card : card list -> int -> card list
(** A list of a number of random cards pulled from a deck, removing each
    card from the deck as it is pulled. *)

val string_of_card : card -> string
(** Prints string representation of card to console *)

val print_deck : card list -> unit
(** Prints string representation of a deck to console *)

val play_deck : unit -> unit
(** Deals 7 random cards and prints string representations of all to
    console *)