open OUnit2
open Deck
open Command
open State

(*************    Pretty Prints   *************)

(** [pp_card c] pretty-prints card [c]. *)
let pp_card (n, col) = 
  "\""^(string_of_int n) ^", " ^col^"\""

(** [pp_deck pp_elt d] pretty-prints deck [d], using [pp_elt]
    to pretty-print each card in [d]. *)
let pp_deck pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let pp_cmd cmd =
  match cmd with
  | Put phrase -> "Play" ^ (pp_list pp_string phrase)
  | Draw -> pp_string "Draw"
  | Quit -> pp_string "Quit"
  | Hand -> pp_string "Hand"
  | _ -> pp_string "unimplemented command"

let pp_state (st:State.t) =
  "\nCurrent Card:\t"^(st|>State.get_current_card|>Deck.list_card|>pp_card)
  ^"\nPlayer's Hand:\t"^(st|>State.get_players_hand|>Deck.to_list|>(pp_deck pp_card))
  ^"\nAI's Hand:\t"^(st|>State.get_ai_hand|>Deck.to_list|>(pp_deck pp_card))
  ^"\nDraw Deck:\t"^(st|>State.get_draw_deck|>Deck.to_list|>(pp_deck pp_card))
  ^"\nPlaying Deck:\t"^(st|>State.get_playing_deck|>Deck.to_list|>(pp_deck pp_card))
  ^"\nTurn:\t"^string_of_bool(State.get_turn st)

(*************    Helpers    *************)

(** [make_exception_test name e f] constructs an OUnit
    test named [name] that asserts the quality of exception thrown by [f]
    with [e]. *)
let make_exception_test
    (name : string)
    (e : exn) 
    (f) =
  name >:: (fun _ -> assert_raises e f)

let rec contains a b =
  match a with 
  | [] -> false
  | h::t -> h = b || contains t b

let rec contains_all a b =
  match b with 
  | [] -> true
  | h::t -> contains a h && contains_all a t

let cmp_deck_lists lst1 lst2 =
  List.length lst1 = List.length lst2
  &&
  contains_all lst1 lst2

let list_format fmt el_format lst = 
  let rec list_format_ = function
    | [] -> Format.fprintf fmt "]"
    | h::[] ->
      el_format fmt h;
      list_format_ []
    | h::t -> 
      el_format fmt h;
      Format.fprintf fmt "; ";
      list_format_ t
  in
  Format.fprintf fmt "[";
  list_format_ lst

let diff fmt formatter (a,b) =
  Format.fprintf fmt "\nExpected:\n ";
  formatter fmt a;
  Format.fprintf fmt "\nBut got:\n ";
  formatter fmt b

let entry_format fmt (n, col) = 
    Format.fprintf fmt "(%d, %s)" n col

let deck_format fmt d =
    list_format fmt entry_format d

let deck_diff fmt (a,b) =
    diff fmt deck_format (a,b)


(*************    Deck Tests    *************)
let test_empty_deck
    (name : string) : test = 
  name >:: (fun _ -> 
      assert_equal [] (Deck.to_list empty_deck) 
         ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

let test_shuffle
    (name : string)
    (d: Deck.t) = 
  let shuffled = shuffle d in
  name >:: (fun _ ->
      let dl = Deck.to_list d in
      let sl = Deck.to_list shuffled in
      assert_equal sl dl 
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

let test_deal
  (name : string)
  (d: Deck.t)  
  (expected: (int*string) list) : test =
  name >:: (fun _ ->
      let (dt, d) = deal d in
      assert_equal expected (Deck.to_list dt)
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

let test_top_card 
    (name : string)
    (d: Deck.t)
    (expected : (int*string)) =
  name >:: (fun _ ->
      let c = top_card d |> list_card in
      assert_equal expected c ~printer:pp_card)

let test_add_card 
    (name : string)
    (d: Deck.t)
    (c: Deck.card)
    (expected: (int*string) list) =
  name >:: (fun _ ->
      let dl = add_card c d |> Deck.to_list in 
      assert_equal expected dl 
        ~cmp:cmp_deck_lists ~pp_diff:(deck_diff))

let test_remove_card 
    (name : string)
    (d: Deck.t)
    (c: Deck.card)
    (expected : (int*string) list) =
  name >:: (fun _ ->
      let dl = remove_card c d |> Deck.to_list in
      assert_equal expected dl 
        ~cmp:cmp_deck_lists ~pp_diff:(deck_diff))

let test_is_valid 
    (name : string)
    (c1: card)
    (c2 : card)
    (expected : bool) =
  name >:: (fun _ ->
      assert_equal expected (is_valid c1 c2) ~printer:string_of_bool)

let test_get_valid_card
    (name : string)
    (d: Deck.t)
    (c: card)
    (expected : (int*string)) =
  name >:: (fun _ ->
    let vc = get_valid_card c d in
    let uc = match vc with
    | None -> (-1, "none")
    | Some x -> list_card x in 
    assert_equal expected uc ~printer:pp_card)

let deck_tests =
let initial_deck = load_deck in
let my_deck = fst (deal initial_deck) in
let ai_deck = fst (deal (snd (deal initial_deck))) in
let remaining = snd (deal (snd (deal initial_deck))) in
let y3 = Deck.create_card "yellow" 3 in
let y4 = Deck.create_card "yellow" 4 in
let r9 = Deck.create_card "red" 9 in
let b9 = Deck.create_card "blue" 9 in
let g5 = Deck.create_card "green" 5 in
let d1 = Deck.add_card y3 Deck.empty_deck in
let d2 = Deck.add_card b9 d1 in
let d3 = Deck.add_card g5 d2 in
    [
      (* Empty deck tests **)
      test_empty_deck "Empty deck test";

      test_add_card "Add card to empty" empty_deck y3 [(3, "yellow")];
      test_add_card "Add card to deck 1" d1 b9 [(9, "blue"); (3, "yellow")];
      test_add_card "Add card to deck 1" d1 y3 [(3, "yellow"); (3, "yellow")];

      (test_deal "Deal loaded deck" load_deck 
        [(3, "red"); (4, "red"); (5, "red"); (6, "red"); (7, "red"); 
         (8, "red"); (9, "red")]);

      test_shuffle "Test shuffle loaded deck" load_deck;
      test_shuffle "Test shuffle deck 1" d1;
      test_shuffle "Test shuffle deck 2" d2;

      test_remove_card "Remove card empty deck" empty_deck y3 [];
      test_remove_card "Remove card d1" d1 y3 [];
      test_remove_card "Remove card not in d1" d1 b9 [(3, "yellow")];
      test_remove_card "Remove card in d3" d3 b9 [(5, "green"); (3, "yellow")];
      test_remove_card "Repeat remove card in d3" 
        (remove_card y3 d3) b9 [(5, "green")];

      test_top_card "Loaded deck top card" load_deck (9, "red");
      test_top_card "Loaded deck top card" d1 (3, "yellow");
      test_top_card "Loaded deck top card" d2 (9, "blue");
      test_top_card "Loaded deck top card" d3 (5, "green");

      test_is_valid "Test valid yellow 3 yellow 4" y3 y4 true;
      test_is_valid "Test valid yellow 4 yellow 3" y4 y3 true;
      test_is_valid "Test valid red 9 blue 9" r9 b9 true;
      test_is_valid "Test valid blue 9 red 9" b9 r9 true;
      test_is_valid "Test invalid green 5 blue 9" g5 b9 false;
      test_is_valid "Test invalid blue 9 green 5" b9 g5 false;

      test_get_valid_card "Get valid card empty deck" empty_deck y3 (-1, "none");
      test_get_valid_card "Get valid card deck 1" d1 y3 (3, "yellow");
      test_get_valid_card "Get valid card deck 1" d1 g5 (-1, "none");
      test_get_valid_card "Get valid card deck 3" d3 y3 (3, "yellow");
      test_get_valid_card "Get valid card deck 3" d3 g5 (5, "green");
      test_get_valid_card "Get valid card deck 3" d3 b9 (9, "blue");
    ]



(*************    Command Tests    *************)

(** [make_parse_test name input_string expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [parse input_string]. *)  
let make_parse_test
    (name : string)
    (input_string : string)
    (expected_output : command) : test = 
  name >:: (fun _ ->
      let real_output = parse input_string in
      assert_equal expected_output real_output ~printer:pp_cmd)


let command_tests =
  [
  make_parse_test "Parse: Quit" "Quit" Quit;
  make_parse_test "Parse: Quit" "  Quit" Quit;
  make_parse_test "Parse: Quit" "Quit  " Quit;
   
  make_parse_test "Parse: Draw" "Draw" Draw;
  make_parse_test "Parse: Draw" "  Draw" Draw;
  make_parse_test "Parse: Draw" "Draw  " Draw;
   
  make_parse_test "Parse: Hand" "Hand" Hand;
  make_parse_test "Parse: Put card" "Put 1 red" (Put ["1"; "red"]);
  make_parse_test "Parse: Put card" " Put  1   red " (Put ["1"; "red"]);
  make_parse_test "Parse: Put card" " Put yellow 3" (Put ["yellow"; "3"]);
   
  make_exception_test "Parse empty exception" 
    (Empty) (fun _ -> parse "");
  make_exception_test "Parse empty exception" 
    (Empty) (fun _ -> parse " ");
  make_exception_test "Parse empty exception" 
    (Empty) (fun _ -> parse "  ");
  make_exception_test "Parse Malformed exception" 
    (Malformed) (fun _ -> parse "adfs");
  make_exception_test "Parse Malformed exception" 
    (Malformed) (fun _ -> parse "Qu it");
  make_exception_test "Parse Malformed exception" 
    (Malformed) (fun _ -> parse "Quit pls");
  make_exception_test "Parse Malformed exception" 
    (Malformed) (fun _ -> parse "Put");
  make_exception_test "Parse Malformed exception" 
    (Malformed) (fun _ -> parse " P ut");
]

(*************    State Tests    *************)

(*let test_get_current_card
    (name: string)
    (st: State.t)
    (expected: Deck.card) =
  name >:: (fun _ ->
    assert_equal expected (State.get_current_card st)~printer:pp_card)

let test_get_players_hand
    (name: string)
    (st: State.t)
    (expected: Deck.t) =
  name >:: (fun _ ->
    assert_equal expected (State.get_players_hand st)~printer:pp_deck)

let test_get_ai_hand
    (name: string)
    (st: State.t)
    (expected: Deck.t) =
  name >:: (fun _ ->
    assert_equal expected (State.get_ai_hand st)~printer:pp_deck)

let test_get_draw_deck
    (name: string)
    (st: State.t)
    (expected: Deck.t) =
  name >:: (fun _ ->
    assert_equal expected (State.get_draw_deck st)~printer:pp_deck)

let test_has_won
    (name: string)
    (st: State.t)
    (expected: bool) =
  name >:: (fun _ ->
    assert_equal expected (State.has_won st)~printer:string_of_bool)

let test_get_turn
    (name: string)
    (st: State.t)
    (expected: bool) =
  name >:: (fun _ ->
    assert_equal expected (State.get_turn st)~printer:string_of_bool)

let test_put
    (name: string)
    (c: Deck.card)
    (st: State.t)
    (s: string)
    (expected: State.t) =
  name >:: (fun _ -> try
    assert_equal expected (State.put c st s)~printer:pp_state
    with Invalid_Move -> assert_raises (Invalid_Move))

let test_draw
    (name: string)
    (st: State.t)
    (s: string)
    (expected: State.t) =
  name >:: (fun _ ->
    assert_equal expected (State.draw st s)~printer:pp_state)*)

(* TODO: Draw tests with reset cases? *)
let state_tests =
  [
   (* let player_deck = [{number = 1; color = Red} ;{number = 2; color = Red}] in
    let ai_deck = [{number = 1; color = Yellow}; {number = 2; color = Yellow}] in
    let current_c = {number = 1; color = Blue} in
    let d_deck = [{number = 1; color = Green}; {number = 2; color = Green}; {number = 3; color = Green }] in
    let p_deck = [{number = 4; color = Blue}; {number = 5; color = Blue}; {number = 6; color = Blue}] in

    let test_state = {current_card = current_c; 
                      players_hand = player_deck; 
                      ai_hand = ai_deck;
                      draw_deck = d_deck;
                      play_deck = p_deck;
                      turn = true} in
    test_get_current_card "Current Card: Blue 1" test_state {number = 1; color = Blue };
    test_get_players_hand "Player Hand: Red 1 2" test_state [{number = 1; color = Red } ;{number = 2; color = Red }];
    test_get_ai_hand "AI Hand: Yellow 1 2" test_state [{number = 1; color = Yellow }; {number = 2; color = Yellow}];
    test_get_draw_deck "Draw: Green 2 3" test_state [{number = 1; color = Green }; {number = 2; color = Green}; {number = 3; color = Green}];
    test_has_won "Has Won: False" test_state false;
    test_get_turn "Turn: True" test_state true;

    let post_player_put = {current_card = {number = 1; color = Red}; 
                      players_hand = [{number = 2; color = Red}]; 
                      ai_hand = ai_deck;
                      draw_deck = d_deck;
                      play_deck = [{number = 1; color = Red}; {number = 1; color = Green }; {number = 2; color = Green}; {number = 3; color = Green}];
                      turn = false} in
    let post_ai_put = {current_card = {number = 1; color = Yellow}; 
                      players_hand = [{number = 2; color = Red}];  
                      ai_hand = [{number = 2; color = Yellow}];
                      draw_deck = d_deck;
                      play_deck = [{number = 1; color = Yellow}; {number = 1; color = Red}; {number = 1; color = Green }; {number = 2; color = Green}; {number = 3; color = Green}];
                      turn = true} in
    test_put "Player Puts Red 1" {number = 1; color = Red} test_state "player" post_player_put;
    test_put "AI Puts Yellow 1" {number = 1; color = Yellow} post_player_put "ai" post_ai_put;
    test_put "Player Puts Invalid" {number = 2; color = Red} post_ai_put "player" post_ai_put;

    let post_player_draw = {current_card = {number = 1; color = Yellow}; 
                      players_hand = [{number = 1; color = Green}; {number = 2; color = Red}];  
                      ai_hand = [{number = 2; color = Yellow}];
                      draw_deck = [{number = 2; color = Green}; {number = 3; color = Green}];
                      play_deck = [{number = 1; color = Yellow}; {number = 1; color = Red}; {number = 1; color = Green }; {number = 2; color = Green}; {number = 3; color = Green}];
                      turn = false} in
    
    let post_ai_draw = {current_card = {number = 1; color = Yellow}; 
                      players_hand = [{number = 1; color = Green}; {number = 2; color = Red}];  
                      ai_hand = [{number = 2; color = Green}; {number = 2; color = Yellow}];
                      draw_deck = [{number = 3; color = Green}];
                      play_deck = [{number = 1; color = Yellow}; {number = 1; color = Red}; {number = 1; color = Green }; {number = 2; color = Green}; {number = 3; color = Green}];
                      turn = true} in
    test_draw "Player Draws Green 1" post_ai_put "player" post_player_draw;
    test_draw "AI Draws Green 1" post_player_draw "ai" post_ai_draw;*)

  ] 
let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    command_tests;
    (*state_tests; *)
  ]

let _ = run_test_tt_main suite