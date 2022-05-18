open OUnit2
open Library

(* Since our project is a essentially text-interface game, many of our
   functions in the different modules relied on manual testing through
   terminal. Specifically, except for Slots and OurDeck, we manually
   tested every outcome for every other module. Another aspect that
   forced us to use manual testing was the use of randomization in
   certain functions, which made it impossible/trivial to truly check
   them.

   Therefore, functionality that does not fall under the above criteria
   in Slots and OurDeck was tested automatically with OUnit. In
   particular, we employed both black box and glass box testing for our
   tests. For example, to compare two cards, we had to first compare two
   identical cards, then two cards with the same value but different
   suits, then two cards with the same suit but different value, and
   finally two cards with the same value and different suits but in
   different order to demonstrate that suits do not have rank. However,
   we also ensured some random test cases too, especially in Ourdeck
   where we had to check that suits did not affect certain aspects of
   our functions such as comparing the values.

   Overall, this form of testing demonstrates correctness for the system
   because not only did we manually test the different possible game and
   action outcomes to match what we expect, we also checked
   functionality in OurDeck and Slots to ensure that all possible cases
   were accounted for. *)

let spade = Deck.Spades
let diamond = Deck.Diamonds
let heart = Deck.Hearts
let club = Deck.Clubs
let card_2ofspades = Deck.make_card spade 2
let card_2ofdiamonds = Deck.make_card diamond 2
let card_2ofhearts = Deck.make_card heart 2
let card_2ofclubs = Deck.make_card club 2
let card_7ofspades = Deck.make_card spade 7
let card_7ofdiamonds = Deck.make_card diamond 7
let card_7ofhearts = Deck.make_card heart 7
let card_7ofclubs = Deck.make_card club 7
let card_aceofspades = Deck.make_card spade 14
let card_aceofdiamonds = Deck.make_card diamond 14
let card_aceofhearts = Deck.make_card heart 14
let card_aceofclubs = Deck.make_card club 14
let card_kingofspades = Deck.make_card spade 13
let card_kingofdiamonds = Deck.make_card diamond 13
let card_kingofhearts = Deck.make_card heart 13
let card_kingofclubs = Deck.make_card club 13
let card_queenofspades = Deck.make_card spade 12
let card_queenofdiamonds = Deck.make_card diamond 12
let card_queenofhearts = Deck.make_card heart 12
let card_queenofclubs = Deck.make_card club 12
let spades_deck = Deck.make_suit spade 2
let diamonds_deck = Deck.make_suit diamond 2
let full_deck = Deck.make_deck

(********************************************************************
   End constants.
 ********************************************************************)

(** [make_card_test name card expected] constructs an OUnit test named
    [name] that asserts the quality of [expected] with
    [Deck.(string_of_card card)]. *)
let make_card_test
    (name : string)
    (card : Deck.card)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    Deck.(string_of_card card)
    ~printer:String.escaped

(** [make_suit_test name suit value expected] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [Deck.string_of_deck (Deck.make_suit suit value)]. *)
let make_suit_test
    (name : string)
    (suit : Deck.suit)
    (value : int)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.string_of_deck (Deck.make_suit suit value))
    ~printer:String.escaped

(** [remove_card_test card_list card expected] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [Deck.string_of_deck (Deck.remove_card card_list card)]. *)
let remove_card_test
    (name : string)
    (card_list : Deck.card list)
    (card : Deck.card)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.string_of_deck (Deck.remove_card card_list card))
    ~printer:String.escaped

(** [compare_card_test card1 card2 expected] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [Deck.compare_card card1 card2]. *)
let compare_card_test
    (name : string)
    (card1 : Deck.card)
    (card2 : Deck.card)
    (expected : int) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.compare_card card1 card2)
    ~printer:string_of_int

(** [deck_without_cards_test cards_list current_deck expected]
    constructs an OUnit test named [name] that asserts the quality of
    [expected] with
    [Deck.string_of_deck
       (Deck.deck_without_cards cards_list current_deck)]. *)
let deck_without_cards_test
    (name : string)
    (cards_list : Deck.card list)
    (current_deck : Deck.card list)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.string_of_deck
       (Deck.deck_without_cards cards_list current_deck))
    ~printer:String.escaped

(** [slots_winnings_test slot1 slot2 slot3] constructs an OUnit test
    named [name] that asserts the quality of [expected] with
    [Slots.slots_winnings slot1 slot2 slot3]. *)
let slots_winnings_test
    (name : string)
    (slot1 : int)
    (slot2 : int)
    (slot3 : int)
    (expected : int) =
  name >:: fun _ ->
  assert_equal expected
    (Slots.slots_winnings slot1 slot2 slot3)
    ~printer:string_of_int

(** [total_winnings_test slot1 slot2 slot3 slot4 slot5 slot6 slot7 slot8 slot9]
    constructs an OUnit test named [name] that asserts the quality of
    [expected] with
    [Deck.string_of_deck
       (Deck.deck_without_cards cards_list current_deck)]. *)
let total_winnings_test
    (name : string)
    (slot1 : int)
    (slot2 : int)
    (slot3 : int)
    (slot4 : int)
    (slot5 : int)
    (slot6 : int)
    (slot7 : int)
    (slot8 : int)
    (slot9 : int)
    (expected : int) =
  name >:: fun _ ->
  assert_equal expected
    (Slots.total_winnings slot1 slot2 slot3 slot4 slot5 slot6 slot7
       slot8 slot9)
    ~printer:string_of_int

(********************************************************************
  End helper functions.
  ********************************************************************)

let ourdeck_tests =
  [
    ( "value_of_card card_2ofspades = 2" >:: fun _ ->
      assert_equal 2 (Deck.value_of_card card_2ofspades) );
    ( "value_of_card card_aceofspades = 14" >:: fun _ ->
      assert_equal 14 (Deck.value_of_card card_aceofspades) );
    ( "exception BadValue \"Out of scope for a deck (value must be \
       between 2 and 14 inclusive)\""
    >:: fun _ ->
      assert_raises
        (Deck.BadValue
           "Out of scope for a deck (value must be between 2 and 14 \
            inclusive)") (fun () -> Deck.make_card spade 1) );
    make_card_test
      "string_of_card(make_card card_2ofspades) = \"2 of Spades\""
      card_2ofspades "2 of Spades";
    make_card_test
      "string_of_card(make_card card_2ofdiamonds) = \"2 of Diamonds\""
      card_2ofdiamonds "2 of Diamonds";
    make_card_test
      "string_of_card(make_card card_2ofhearts) = \"2 of Hearts\""
      card_2ofhearts "2 of Hearts";
    make_card_test
      "string_of_card(make_card card_2ofclubs) = \"2 of Clubs\""
      card_2ofclubs "2 of Clubs";
    make_card_test
      "string_of_card(make_card card_aceofspades)= \"Ace of Spades\""
      card_aceofspades "Ace of Spades";
    make_card_test
      "string_of_card(make_card card_aceofdiamonds) = \"Ace of \
       Diamonds\""
      card_aceofdiamonds "Ace of Diamonds";
    make_card_test
      "string_of_card(make_card card_aceofhearts) = \"Ace of Hearts\""
      card_aceofhearts "Ace of Hearts";
    make_card_test
      "string_of_card(make_card card_aceofclubs) = \"Ace of Clubs\""
      card_aceofclubs "Ace of Clubs";
    make_card_test
      "string_of_card(make_card card_kingofspades) = \"King of Spades\""
      card_kingofspades "King of Spades";
    make_card_test
      "string_of_card(make_card card_kingofdiamonds) = \"King of \
       Diamonds\""
      card_kingofdiamonds "King of Diamonds";
    make_card_test
      "string_of_card(make_card card_kingofhearts) = \"King of Hearts\""
      card_kingofhearts "King of Hearts";
    make_card_test
      "string_of_card(make_card card_kingofclubs) = \"King of Clubs\""
      card_kingofclubs "King of Clubs";
    make_card_test
      "string_of_card(make_card card_queenofspades) = \"Queen of \
       Spades\""
      card_queenofspades "Queen of Spades";
    make_card_test
      "make_card card_queenofdiamonds =\"Queen of Diamonds\""
      card_queenofdiamonds "Queen of Diamonds";
    make_card_test "make_card card_queenofhearts = \"Queen of Hearts\""
      card_queenofhearts "Queen of Hearts";
    make_card_test "make_card card_queenofclubs = \"Queen of Clubs\""
      card_queenofclubs "Queen of Clubs";
    ( "exception BadValue \"Out of scope for make_suit (value must be \
       between 2 and 15 inclusive)\""
    >:: fun _ ->
      assert_raises
        (Deck.BadValue
           "Out of scope for make_suit (value must be between 2 and 15 \
            inclusive)") (fun () -> Deck.make_suit spade 1) );
    make_suit_test "string_of_deck(make_suit spade 15) = \"\"" spade 15
      "";
    make_suit_test
      "string_of_deck(make_suit spade 14) = \"Ace of Spades\n\"" spade
      14 "Ace of Spades\n";
    make_suit_test
      "string_of_deck(make_suit spade 13) = \"King of Spades\n\
       Ace of Spades\n\
       \""
      spade 13 "King of Spades\nAce of Spades\n";
    make_suit_test "create deck of spades cards, with one value of each"
      spade 2
      "2 of Spades\n\
       3 of Spades\n\
       4 of Spades\n\
       5 of Spades\n\
       6 of Spades\n\
       7 of Spades\n\
       8 of Spades\n\
       9 of Spades\n\
       10 of Spades\n\
       Jack of Spades\n\
       Queen of Spades\n\
       King of Spades\n\
       Ace of Spades\n";
    remove_card_test "remove 2 of spades from an empty deck of cards" []
      card_2ofspades "";
    remove_card_test
      "remove 2 of diamonds from a deck with only 2 of spades"
      [ card_2ofspades ] card_2ofdiamonds "2 of Spades\n";
    remove_card_test
      "remove 2 of hearts from a deck with only all 13 spades"
      (Deck.make_suit spade 2)
      card_2ofhearts
      (Deck.string_of_deck spades_deck);
    remove_card_test "remove ace of spades from a full 52 card deck"
      full_deck card_aceofspades
      "2 of Spades\n\
       3 of Spades\n\
       4 of Spades\n\
       5 of Spades\n\
       6 of Spades\n\
       7 of Spades\n\
       8 of Spades\n\
       9 of Spades\n\
       10 of Spades\n\
       Jack of Spades\n\
       Queen of Spades\n\
       King of Spades\n\
       2 of Diamonds\n\
       3 of Diamonds\n\
       4 of Diamonds\n\
       5 of Diamonds\n\
       6 of Diamonds\n\
       7 of Diamonds\n\
       8 of Diamonds\n\
       9 of Diamonds\n\
       10 of Diamonds\n\
       Jack of Diamonds\n\
       Queen of Diamonds\n\
       King of Diamonds\n\
       Ace of Diamonds\n\
       2 of Hearts\n\
       3 of Hearts\n\
       4 of Hearts\n\
       5 of Hearts\n\
       6 of Hearts\n\
       7 of Hearts\n\
       8 of Hearts\n\
       9 of Hearts\n\
       10 of Hearts\n\
       Jack of Hearts\n\
       Queen of Hearts\n\
       King of Hearts\n\
       Ace of Hearts\n\
       2 of Clubs\n\
       3 of Clubs\n\
       4 of Clubs\n\
       5 of Clubs\n\
       6 of Clubs\n\
       7 of Clubs\n\
       8 of Clubs\n\
       9 of Clubs\n\
       10 of Clubs\n\
       Jack of Clubs\n\
       Queen of Clubs\n\
       King of Clubs\n\
       Ace of Clubs\n";
    compare_card_test "comparing two identical cards" card_2ofclubs
      card_2ofclubs 0;
    compare_card_test
      "comparing two cards with the same value but different suits"
      card_2ofclubs card_2ofhearts 0;
    compare_card_test
      "comparing two cards with different values (c1>c2) but same suit"
      card_7ofclubs card_2ofclubs 1;
    compare_card_test
      "comparing two cards with different values (c1<c2) but same suit"
      card_2ofclubs card_7ofclubs (-1);
    compare_card_test
      "comparing two cards with different values and suits (c1>c2)"
      card_7ofhearts card_2ofclubs 1;
    compare_card_test
      "comparing two cards with different values and suits (c1<c2)"
      card_2ofclubs card_7ofhearts (-1);
    compare_card_test
      "comparing two cards with different values and suits (c1>c2) to \
       show that suits have no rank"
      card_7ofclubs card_2ofhearts 1;
    compare_card_test
      "comparing two cards with different values and suits (c1<c2) to \
       show that suits have no rank"
      card_2ofhearts card_7ofclubs (-1);
    deck_without_cards_test
      "filtering an empty card deck with an empty card deck" [] [] "";
    deck_without_cards_test
      "filtering an empty card deck with a deck with 2 of spades"
      [ card_2ofspades ] [] "";
    deck_without_cards_test
      "filtering a deck with only 2 of spades with a deck with only 2 \
       of spades"
      [ card_2ofspades ] [ card_2ofspades ] "";
    deck_without_cards_test
      "filtering a deck with only 2 of clubs with a deck with only 2 \
       of spades"
      [ card_2ofclubs ] [ card_2ofspades ] "2 of Spades\n";
    deck_without_cards_test
      "filtering a deck with only 2 of spades with a spades-only deck"
      spades_deck [ card_2ofspades ] "";
    deck_without_cards_test
      "filtering a deck with only 2 of spades with a diamonds-only \
       deck "
      diamonds_deck [ card_2ofspades ] "2 of Spades\n";
    deck_without_cards_test
      "filtering a spades-only deck with a spades-only" spades_deck
      spades_deck "";
    deck_without_cards_test
      "filtering a spades-only deck with a diamonds-only" diamonds_deck
      spades_deck
      (Deck.string_of_deck spades_deck);
    deck_without_cards_test
      "filtering a full 52-card deck with a diamonds-only deck"
      diamonds_deck full_deck
      "2 of Spades\n\
       3 of Spades\n\
       4 of Spades\n\
       5 of Spades\n\
       6 of Spades\n\
       7 of Spades\n\
       8 of Spades\n\
       9 of Spades\n\
       10 of Spades\n\
       Jack of Spades\n\
       Queen of Spades\n\
       King of Spades\n\
       Ace of Spades\n\
       2 of Hearts\n\
       3 of Hearts\n\
       4 of Hearts\n\
       5 of Hearts\n\
       6 of Hearts\n\
       7 of Hearts\n\
       8 of Hearts\n\
       9 of Hearts\n\
       10 of Hearts\n\
       Jack of Hearts\n\
       Queen of Hearts\n\
       King of Hearts\n\
       Ace of Hearts\n\
       2 of Clubs\n\
       3 of Clubs\n\
       4 of Clubs\n\
       5 of Clubs\n\
       6 of Clubs\n\
       7 of Clubs\n\
       8 of Clubs\n\
       9 of Clubs\n\
       10 of Clubs\n\
       Jack of Clubs\n\
       Queen of Clubs\n\
       King of Clubs\n\
       Ace of Clubs\n";
  ]

let slots_tests =
  [
    slots_winnings_test "1 1 1 wins 1000" 1 1 1 1000;
    slots_winnings_test "1 0 1 wins 0" 1 0 1 0;
    slots_winnings_test "1 1 0 wins 0" 1 1 0 0;
    total_winnings_test "1 2 3 4 5 6 7 8 9 wins 0" 1 2 3 4 5 6 7 8 9 0;
    total_winnings_test "1 0 1 0 2 0 1 0 1 wins 0" 1 0 1 0 2 0 1 0 1 0;
    total_winnings_test "1 1 0 1 2 1 0 1 1 wins 0" 1 1 0 1 2 1 0 1 1 0;
    total_winnings_test "1 1 1 4 5 6 7 8 9 wins 1000" 1 1 1 4 5 6 7 8 9
      1000;
    total_winnings_test "1 2 3 4 4 4 7 8 9 wins 1000" 1 2 3 4 4 4 7 8 9
      1000;
    total_winnings_test "1 2 3 4 5 6 7 7 7 wins 1000" 1 2 3 4 5 6 7 7 7
      1000;
    total_winnings_test "1 2 3 1 5 6 1 8 9 wins 1000" 1 2 3 1 5 6 1 8 9
      1000;
    total_winnings_test "1 2 3 4 2 6 7 2 9 wins 1000" 1 2 3 4 2 6 7 2 9
      1000;
    total_winnings_test "1 2 3 4 5 3 7 8 3 wins 1000" 1 2 3 4 5 3 7 8 3
      1000;
    total_winnings_test "1 2 3 4 1 6 7 8 1 wins 1000" 1 2 3 4 1 6 7 8 1
      1000;
    total_winnings_test "1 2 3 4 3 6 3 8 9 wins 1000" 1 2 3 4 3 6 3 8 9
      1000;
    total_winnings_test "1 1 1 4 4 4 7 8 9 wins 2000" 1 1 1 4 4 4 7 8 9
      2000;
    total_winnings_test "1 1 1 4 5 6 7 7 7 wins 2000" 1 1 1 4 5 6 7 7 7
      2000;
    total_winnings_test "1 2 3 4 4 4 7 7 7 wins 2000" 1 2 3 4 4 4 7 7 7
      2000;
    total_winnings_test "1 2 3 1 2 6 1 2 9 wins 2000" 1 2 3 1 2 6 1 2 9
      2000;
    total_winnings_test "1 2 3 4 2 3 7 2 3 wins 2000" 1 2 3 4 2 3 7 2 3
      2000;
    total_winnings_test "1 2 3 1 5 3 1 8 3 wins 2000" 1 2 3 1 5 3 1 8 3
      2000;
    total_winnings_test "1 2 1 4 1 6 1 8 1 wins 2000" 1 2 1 4 1 6 1 8 1
      2000;
    total_winnings_test "1 1 1 4 4 4 7 7 7 wins 3000" 1 1 1 4 4 4 7 7 7
      3000;
    total_winnings_test "1 2 3 1 2 3 1 2 3 wins 3000" 1 2 3 1 2 3 1 2 3
      3000;
    total_winnings_test "1 1 1 1 0 1 1 1 1 wins 4000" 1 1 1 1 0 1 1 1 1
      4000;
    total_winnings_test "1 0 1 1 1 1 1 0 1 wins 5000" 1 0 1 1 1 1 1 0 1
      5000;
    total_winnings_test "1 1 1 0 1 0 1 1 1 wins 4000" 1 1 1 0 1 0 1 1 1
      5000;
    total_winnings_test "1 1 1 1 1 1 1 0 1 wins 6000" 1 1 1 1 1 1 1 0 1
      6000;
    total_winnings_test "1 0 1 1 1 1 1 1 1 wins 6000" 1 0 1 1 1 1 1 1 1
      6000;
    total_winnings_test "1 1 1 1 1 1 1 1 1 wins 8000" 1 1 1 1 1 1 1 1 1
      8000;
  ]

let tests =
  "test suite for A1" >::: List.flatten [ ourdeck_tests; slots_tests ]

let _ = run_test_tt_main tests