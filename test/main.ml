open OUnit2
open Library

(** [write the test plan shit here]*)

let spade = Deck.Spades
let diamond = Deck.Diamonds
let heart = Deck.Hearts
let club = Deck.Clubs
let card_2ofspades = Deck.make_card spade 2
let card_2ofdiamonds = Deck.make_card diamond 2
let card_2ofhearts = Deck.make_card heart 2
let card_2ofclubs = Deck.make_card club 2
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
let full_deck = Deck.make_deck

let make_card_test
    (name : string)
    (card : Deck.card)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    Deck.(string_of_card card)
    ~printer:String.escaped

let make_suit_test
    (name : string)
    (suit : Deck.suit)
    (value : int)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.string_of_deck (Deck.make_suit suit value))
    ~printer:String.escaped

let remove_card_test
    (name : string)
    (card_list : Deck.card list)
    (card : Deck.card)
    (expected : string) =
  name >:: fun _ ->
  assert_equal expected
    (Deck.string_of_deck (Deck.remove_card card_list card))
    ~printer:String.escaped

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
  ]

let tests = "test suite for A1" >::: List.flatten [ ourdeck_tests ]
let _ = run_test_tt_main tests