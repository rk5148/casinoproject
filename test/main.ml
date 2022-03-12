(* open OUnit2
open Game
open Adventure
open Command
open State
open Yojson.Basic.Util

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list]
   to get helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo";
       "foo"] ["foo"]); *);
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as
   the basis for unit tests. You can also use the JSON files in the data
   directory as tests. And you can add JSON files in this directory and
   use them, too. *)

(* You should not be testing any helper functions here. Test only the
   functions exposed in the [.mli] files. Do not expose your helper
   functions. See the handout for an explanation. *)

(* TODO: add unit tests for modules below. You are free to reorganize
   the definitions below. Just keep it clear which tests are for which
   modules. *)

let ho_plaza_json =
  Yojson.Basic.from_file "/home/ayagnik/3110/a2/a2/data/ho_plaza.json"

let ho_plaza_adv = Adventure.from_json ho_plaza_json

let lonely_room_json =
  Yojson.Basic.from_file
    "/home/ayagnik/3110/a2/a2/data/lonely_room.json"

let lonely_room_adv = Adventure.from_json lonely_room_json

let try_test (name : string) testing correct : test =
  name >:: fun _ ->
  assert_equal true (testing = correct) ~printer:string_of_bool

let start_room_tests =
  [
    try_test "start room is ho plaza"
      (Adventure.start_room ho_plaza_adv)
      "ho plaza";
    try_test "start room is the room"
      (Adventure.start_room lonely_room_adv)
      "the room";
  ]

let room_ids_tests =
  [
    try_test "room id's are [ho plaza; health; tower; nirvana]"
      (Adventure.room_ids ho_plaza_adv)
      [ "ho plaza"; "health"; "tower"; "nirvana" ];
    try_test "room id's are [the room]"
      (Adventure.room_ids lonely_room_adv)
      [ "the room" ];
  ]

let desc_tests =
  [
    try_test
      "description of ho plaza is 'You are on ... you avoid them'"
      (Adventure.description ho_plaza_adv "ho plaza")
      "You are on Ho Plaza. Cornell Health is to the southwest. The \
       chimes are playing a concert in the clock tower. Someone tries \
       to hand you a quartercard, but you avoid them.";
    try_test
      "description of health is 'You are at ... to the northeast'"
      (Adventure.description ho_plaza_adv "health")
      "You are at the entrance to Cornell Health. A sign advertises \
       free flu shots. You briefly wonder how long it would take to \
       get an appointment. Ho Plaza is to the northeast.";
    try_test
      "description of tower is 'You climbed up ... to ascend higher'"
      (Adventure.description ho_plaza_adv "tower")
      "You climbed up all 161 steps to the top of McGraw Tower. A \
       Chimesmaster is playing the Jennie McGraw Rag. You feel \
       inspired to ascend higher.";
    try_test
      "description of nirvana is 'You have reached ... no more words'"
      (Adventure.description ho_plaza_adv "nirvana")
      "You have reached a higher level of existence.  There are no \
       more words.";
    try_test "description of the room is 'A very lonely room.'"
      (Adventure.description lonely_room_adv "the room")
      "A very lonely room.";
  ]

let exit_tests =
  [
    try_test
      "exits from ho plaza are [southwest, south west, Cornell Health, \
       Gannett, chimes, concert, clock tower]"
      (Adventure.exits ho_plaza_adv "ho plaza")
      [
        "southwest";
        "south west";
        "Cornell Health";
        "Gannett";
        "chimes";
        "concert";
        "clock tower";
      ];
    try_test "exits from health are [northeast, north east, Ho Plaza]"
      (Adventure.exits ho_plaza_adv "health")
      [ "northeast"; "north east"; "Ho Plaza" ];
    try_test "exits from tower are [down, back, Ho Plaza, higher]"
      (Adventure.exits ho_plaza_adv "tower")
      [ "down"; "back"; "Ho Plaza"; "higher" ];
    try_test "exits from nirvana are []"
      (Adventure.exits ho_plaza_adv "nirvana")
      [];
    try_test "exits from the room are []"
      (Adventure.exits lonely_room_adv "the room")
      [];
  ]

let next_room_tests =
  [
    try_test "move from ho plaza to health"
      (Adventure.next_room ho_plaza_adv "ho plaza" "southwest")
      "health";
    try_test "move from ho plaza to tower"
      (Adventure.next_room ho_plaza_adv "ho plaza" "concert")
      "tower";
    try_test "move from tower to ho plaza"
      (Adventure.next_room ho_plaza_adv "tower" "down")
      "ho plaza";
    try_test "move from tower to nirvana"
      (Adventure.next_room ho_plaza_adv "tower" "higher")
      "nirvana";
    try_test "move from health to ho plaza"
      (Adventure.next_room ho_plaza_adv "health" "northeast")
      "ho plaza";
  ]

let next_rooms_tests =
  [
    try_test "Next rooms from ho plaza are [health, tower]"
      (Adventure.next_rooms ho_plaza_adv "ho plaza")
      [ "health"; "tower" ];
    try_test "Next rooms from health are [ho plaza]"
      (Adventure.next_rooms ho_plaza_adv "health")
      [ "ho plaza" ];
    try_test "Next rooms from tower are [ho plaza, nirvana]"
      (Adventure.next_rooms ho_plaza_adv "tower")
      [ "ho plaza"; "nirvana" ];
    try_test "Next rooms from nirvana are []"
      (Adventure.next_rooms ho_plaza_adv "nirvana")
      [];
  ]

let adventure_tests =
  List.flatten
    [
      start_room_tests;
      room_ids_tests;
      desc_tests;
      exit_tests;
      next_room_tests;
      next_rooms_tests;
    ]

let parse_test
    (name : string)
    (movement : string)
    (correct_comm : command) : test =
  name >:: fun _ -> assert_equal (Command.parse movement) correct_comm

let parse_test_exception
    (name : string)
    (movement : string)
    (correct_exc : exn) : test =
  name >:: fun _ ->
  assert_raises correct_exc (fun () -> Command.parse movement)

let command_tests =
  [
    parse_test "Should quit" "quit" Quit;
    parse_test "Should go to clock tower" "go clock tower"
      (Go [ "clock"; "tower" ]);
    parse_test "Should go to clock tower" "go     clock      tower"
      (Go [ "clock"; "tower" ]);
    parse_test_exception "Should raise Empty" "" Empty;
    parse_test_exception "Should raise Empty" "            " Empty;
    parse_test_exception "Should raise Malformed" "Go Clock Tower"
      Malformed;
    parse_test_exception "Should raise Malformed" "quit clock tower"
      Malformed;
    parse_test_exception "Should raise Malformed" "go" Malformed;
  ]

let illegal_state_test
    (name : string)
    (ex : string)
    (adv : Adventure.t)
    (st : State.t)
    (correct : result) : test =
  name >:: fun _ -> assert_equal (State.go ex adv st) correct

let legal_state_partial_test output new_room_id visited_rooms =
  let remove_legality =
    match output with
    | Legal t -> t
    | _ -> failwith "This output is invalid"
  in
  if
    State.current_room_id remove_legality = new_room_id
    && cmp_set_like_lists (State.visited remove_legality) visited_rooms
  then true
  else false

let state_tests =
  [
    illegal_state_test "Test an illegal entry" "northeast" ho_plaza_adv
      (State.init_state ho_plaza_adv)
      Illegal;
    ( "Test a legal entry" >:: fun _ ->
      assert (
        legal_state_partial_test
          (State.go "southwest" ho_plaza_adv
             (State.init_state ho_plaza_adv))
          "health"
          [ "health"; "ho plaza" ]) );
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ adventure_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite *)
