exception BadValue of string
exception Empty
exception NotFound

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

val value_of_card : card -> int
(** [value_of_card card] is the numerical value of the card. Requires:
    [card] is a valid card. *)

val compare_card : card -> card -> int
(** [compare_card card1 card2 ] is [1] if value_of_card card1 >
    value_of_card card2, [-1] if value_of_card card1 < value_of_card
    card2, or [0] if value_of_card card1 = value_of_card card2.
    Requires: the values of each card should be valid card values (2 <=
    v <= 14).*)

val make_card : suit -> int -> card
(**[make_card suit value] creates a card with given [suit] and [value].
   Requires: [card] is a valid card, 2 <= [value] <= 14. *)

val make_suit : suit -> int -> card list
(**[make_suit suit value] fills the given suit from card value [value]
   to card value 14 ([value] to Ace). Requires: [card] is a valid card,
   2 <= [value] <= 14. *)

val make_deck : card list
(** A deck of 52 cards with all 13 number and face cards for all four
    suits. (Note: this means the 52 cards are all distinct.)*)

val remove_card : card list -> card -> card list
(** [remove_card deck card] removes [card] from [deck]. If [card] is not
    found, it returns [deck]. Requires: [card] is a valid card and
    [deck] is a valid deck with no duplicate cards. *)

val pull_card : card list -> int -> card list
(** [pull card deck number] is a deck of a number of random cards
    determined by [number] pulled from the deck [deck], removing each
    card from the deck [deck] as it is pulled. Requires: [deck] is a
    valid deck with no duplicate cards. *)

val string_of_card : card -> string
(** [string_of_card card] is the string representation of card to
    console. Requires: [card] is a valid card. *)

val string_of_deck : card list -> string
(** [string_of_deck deck] is the string representation of a deck to
    console. Requires: [deck] is a valid deck with no duplicate cards. *)

val play_deck : unit -> unit
(** [play_deck] deals 7 random cards and prints string representations
    of all to console *)

val deck_without_cards : card list -> card list -> card list
(** [deck_without_cards cards_list current_deck] filters [current_deck]
    by removing all cards it has in common with [cards_list]. [deck] is
    a valid deck with no duplicate cards. *)

val pull_two_compare : string -> string -> int
(** [pull_two_compare player1 player2] deals a random card from a full
    deck of 52 cards to player1 and player2, announces those cards, and
    compares them, returning either : 1 (if p1 > p2), -1 (if p1 < p2),
    or 0 (if p1=p2) *)
