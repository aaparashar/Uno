open OUnit2
open Deck
open Command
open State

(*************    Pretty Prints   *************)

(** [pp_card c] pretty-prints card [c]. *)
let pp_card (v, col) = 
  "\""^v ^", " ^col^"\""

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

(** [pp_cmd cmd] pretty-prints command [cmd], as a string *)
let pp_cmd cmd =
  match cmd with
  | Put phrase -> "Play" ^ (pp_list pp_string phrase)
  | Draw -> pp_string "Draw"
  | Quit -> pp_string "Quit"
  | Hand -> pp_string "Hand"
  | _ -> pp_string "unimplemented command"

(** [pp_state  st] pretty-prints each attribute of state [st], as a string *)
let pp_state (st:State.t) =
  "\nCurrent Card:\t"^(st|>State.get_current_card|>deck.list_card|>pp_card)
  ^"\nPlayer's Hand:\t"^(st|>State.get_players_hand|>deck.to_list|>(pp_deck pp_card))
  ^"\nAI's Hand:\t"^(st|>State.get_ai_hand|>deck.to_list|>(pp_deck pp_card))
  ^"\nDraw deck:\t"^(st|>State.get_draw_deck|>deck.to_list|>(pp_deck pp_card))
  ^"\nPlaying deck:\t"^(st|>State.get_playing_deck|>deck.to_list|>(pp_deck pp_card))
  ^"\nPlayer Has Played:\t"^(st|>State.get_player_played|>deck.to_list|>(pp_deck pp_card))
  ^"\nLast Player Action:\t"^(st|>State.get_lastp_action)
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

(**[contains a b] returns true if b has an element that is equal to a and false 
   otherwise *)
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

let entry_format fmt (v, col) = 
  Format.fprintf fmt "(%s, %s)" v col

let deck_format fmt d =
  list_format fmt entry_format d

let deck_diff fmt (a,b) =
  diff fmt deck_format (a,b)


(*************    deck Tests    *************)

(*TODO: edit types on test functions to include power and number cards *)

(** [test_empty_deck name] constructs an OUnit test named [name] that 
    asserts the quality of [[]]] with [deck.to_list empty_deck]. *)  
let test_empty_deck
    (name : string) : test = 
  name >:: (fun _ -> 
      assert_equal [] (deck.to_list empty_deck) 
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_shuffle name d] constructs an OUnit test named [name] that 
    asserts the quality of [deck.to_list (shuffle d)] with 
    [deck.to_list d]. *)  
let test_shuffle
    (name : string)
    (d: deck.t) = 
  let shuffled = shuffle d in
  name >:: (fun _ ->
      let dl = deck.to_list d in
      let sl = deck.to_list shuffled in
      assert_equal sl dl 
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_deal name d expected] constructs an OUnit test named [name] that 
    asserts the quality of [deck.to_list (fst (deal d))] with [expected]. *)  
let test_deal
    (name : string)
    (d: deck.t)  
    (expected: (string*string) list) : test =
  name >:: (fun _ ->
      assert_equal expected (deck.to_list (fst (deal d)))
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_top_card name d expected] constructs an OUnit test named [name] 
    that asserts the quality of [top_card d |> list_card] with [expected]. *)
let test_top_card 
    (name : string)
    (d: deck.t)
    (expected : (string*string)) =
  name >:: (fun _ ->
      let c = top_card d |> list_card in
      assert_equal expected c ~printer:pp_card)

(** [test_add_card name d c expected] constructs an OUnit test named [name] 
    that asserts the quality of [add_card c d |> deck.to_list] with 
    [expected]. *)
let test_add_card 
    (name : string)
    (d: deck.t)
    (c: deck.card)
    (expected: (string*string) list) =
  name >:: (fun _ ->
      let dl = add_card c d |> deck.to_list in 
      assert_equal expected dl 
        ~cmp:cmp_deck_lists ~pp_diff:(deck_diff))

(** [test_remove_card name d c expected] constructs an OUnit test named 
    [name] that asserts the quality of [remove_card c d |> deck.to_list] 
    with [expected]. *)
let test_remove_card 
    (name : string)
    (d: deck.t)
    (c: deck.card)
    (expected : (string*string) list) =
  name >:: (fun _ ->
      let dl = remove_card c d |> deck.to_list in
      assert_equal expected dl 
        ~cmp:cmp_deck_lists ~pp_diff:(deck_diff))

(** [test_is_valid name c1 c2 expected] constructs an OUnit test named 
    [name] that asserts the quality of [is_valid c1 c2] with [expected]. *)
let test_is_valid 
    (name : string)
    (c1: card)
    (c2 : card)
    (expected : bool) =
  name >:: (fun _ ->
      assert_equal expected (is_valid c1 c2) ~printer:string_of_bool)

(** [test_contains name c d expected] constructs an OUnit test named 
    [name] that asserts the quality of [deck_contains c d] with [expected]. *)
let test_contains
    (name : string)
    (c: card)
    (d : deck.t)
    (expected : bool) =
  name >:: (fun _ ->
      assert_equal expected (deck_contains c d) ~printer:string_of_bool)

(** [test_get_valid_card] constructs an OUnit test named [name] that 
    asserts the quality of [get_valid_card c d] with [expected]. *)
let test_get_valid_card
    (name : string)
    (d: deck.t)
    (c: card)
    (expected : (string*string)) =
  name >:: (fun _ ->
      let vc = get_valid_card c d in
      let uc = match vc with
        | None -> ("-1", "none")
        | Some x -> list_card x in 
      assert_equal expected uc ~printer:pp_card)

let deck_tests =
  let initial_deck = load_deck in
  let my_deck = fst (deal initial_deck) in
  let ai_deck = fst (deal (snd (deal initial_deck))) in
  let remaining = snd (deal (snd (deal initial_deck))) in
  let y3 = deck.create_num_card "yellow" 3 in
  let y4 = deck.create_num_card "yellow" 4 in
  let r9 = deck.create_num_card "red" 9 in
  let b9 = deck.create_num_card "blue" 9 in
  let g5 = deck.create_num_card "green" 5 in
  let yrev = deck.create_pow_card "yellow" "reverse" in 
  let bskip = deck.create_pow_card "blue" "skip" in 
  let rd2 = deck.create_pow_card "red" "draw two" in
  let w = deck.create_pow_card "wild" "wild" in 
  let wd4 = deck.create_pow_card "wild" "draw four" in 
  let d1 = deck.add_card y3 deck.empty_deck in
  let d2 = deck.add_card b9 d1 in
  let d3 = deck.add_card g5 d2 in
  [
    (* Empty deck tests **)
    test_empty_deck "Empty deck test";

    test_add_card "Add card to empty" empty_deck y3 [("3", "yellow")];
    test_add_card "Add card to deck 1" d1 b9 [("9", "blue"); ("3", "yellow")];
    test_add_card "Add card to deck 1" d1 y3 [("3", "yellow"); ("3", "yellow")];

    (test_deal "Deal loaded deck" load_deck 
       [("3", "red"); ("4", "red"); ("5", "red"); ("6", "red"); ("7", "red"); 
        ("8", "red"); ("9", "red")]);

    test_shuffle "Test shuffle loaded deck" load_deck;
    test_shuffle "Test shuffle deck 1" d1;
    test_shuffle "Test shuffle deck 2" d2;

    test_remove_card "Remove card empty deck" empty_deck y3 [];
    test_remove_card "Remove card d1" d1 y3 [];
    test_remove_card "Remove card not in d1" d1 b9 [("3", "yellow")];
    test_remove_card "Remove card in d3" d3 b9 [("5", "green"); ("3", "yellow")];
    test_remove_card "Repeat remove card in d3" 
      (remove_card y3 d3) b9 [("5", "green")];

    test_top_card "Loaded deck top card" load_deck ("9", "red");
    test_top_card "Loaded deck top card" d1 ("3", "yellow");
    test_top_card "Loaded deck top card" d2 ("9", "blue");
    test_top_card "Loaded deck top card" d3 ("5", "green");

    test_is_valid "Test valid yellow 3 yellow 4" y3 y4 true;
    test_is_valid "Test valid yellow 4 yellow 3" y4 y3 true;
    test_is_valid "Test valid red 9 blue 9" r9 b9 true;
    test_is_valid "Test valid blue 9 red 9" b9 r9 true;
    test_is_valid "Test invalid green 5 blue 9" g5 b9 false;
    test_is_valid "Test invalid blue 9 green 5" b9 g5 false;

    test_contains "Test contains deck 1" y3 d1 true;
    test_contains "Test contains deck 3" g5 d3 true;
    test_contains "Test contains deck 3" b9 d3 true;
    test_contains "Test contains deck 3" y4 d3 false;
    test_contains "Test contains deck 3" r9 d3 false;
    test_contains "Test contains empty deck" y3 empty_deck false;

    test_get_valid_card "Get valid card empty deck" empty_deck y3 ("-1", "none");
    test_get_valid_card "Get valid card deck 1" d1 y3 ("3", "yellow");
    test_get_valid_card "Get valid card deck 1" d1 g5 ("-1", "none");
    test_get_valid_card "Get valid card deck 3" d3 y3 ("3", "yellow");
    test_get_valid_card "Get valid card deck 3" d3 g5 ("5", "green");
    test_get_valid_card "Get valid card deck 3" d3 b9 ("9", "blue");
  ]



(*************    Command Tests    *************)

(** [make_parse_test name input_string expected] constructs an OUnit
    test named [name] that asserts the quality of [expected]
    with [parse input_string]. *)  
let make_parse_test
    (name : string)
    (input_string : string)
    (expected : command) : test = 
  name >:: (fun _ ->
      let real_output = parse input_string in
      assert_equal expected real_output ~printer:pp_cmd)


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

(** [create_deck d cards] is deck [d] with the cards in card list [cards]
    added to it. *)
let rec create_deck d cards = 
  match cards with
  |[] -> d
  |h::t -> create_deck (add_card h d) t

(** [test_get_current_card name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [list_card (State.get_current_card st)] 
    with [expected]. *)
let test_get_current_card
    (name: string)
    (st: State.t)
    (expected: (string*string)) =
  name >:: (fun _ ->
      assert_equal expected (list_card (State.get_current_card st)) ~printer:pp_card)


(** [test_get_players_hand name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [deck.to_list (State.get_players_hand st)] 
    with [expected]. *)
let test_get_players_hand
    (name: string)
    (st: State.t)
    (expected: (string*string) list) =
  name >:: (fun _ ->
      assert_equal expected (deck.to_list (State.get_players_hand st)) 
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_get_ai_hand name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [deck.to_list (State.get_ai_hand st)] 
    with [expected]. *)
let test_get_ai_hand
    (name: string)
    (st: State.t)
    (expected: (string*string) list) =
  name >:: (fun _ ->
      assert_equal expected (deck.to_list (State.get_ai_hand st))
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_get_draw_deck name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [deck.to_list (State.get_draw_deck st)] 
    with [expected]. *)
let test_get_draw_deck
    (name: string)
    (st: State.t)
    (expected: (string*string) list) =
  name >:: (fun _ ->
      assert_equal expected (deck.to_list (State.get_draw_deck st))
        ~cmp:cmp_deck_lists ~pp_diff:deck_diff)

(** [test_has_won name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [State.has_won st] with [expected]. *)
let test_has_won
    (name: string)
    (st: State.t)
    (expected: bool) =
  name >:: (fun _ ->
      assert_equal expected (State.has_won st)~printer:string_of_bool)

(** [test_get_turn name st expected] constructs an OUnit test named 
    [name] that asserts the quality of [State.get_turn st] with [expected]. *)
let test_get_turn
    (name: string)
    (st: State.t)
    (expected: bool) =
  name >:: (fun _ ->
      assert_equal expected (State.get_turn st)~printer:string_of_bool)

(** [test_put name c st s expected] constructs an OUnit test named 
    [name] that asserts the quality of [State.put c st s] with [expected]. *)
let test_put
    (name: string)
    (c: deck.card)
    (st: State.t)
    (s: string)
    (expected: State.t) =
  name >:: (fun _ -> 
      assert_equal expected (State.put c st s) ~printer:pp_state)

(** [test_draw name st s expected] constructs an OUnit test named 
    [name] that asserts the quality of [State.draw st s] with [expected]. *)
let test_draw
    (name: string)
    (st: State.t)
    (s: string)
    (expected: State.t) =
  name >:: (fun _ ->
      assert_equal expected (State.draw st s)~printer:pp_state)

(* TODO: Draw tests with reset cases? *)
let state_tests =
  let r2 = create_num_card "red" 2 in
  let r1 = create_num_card "red" 1 in
  let y2 = create_num_card  "yellow" 2 in
  let y1 = create_num_card  "yellow" 1 in
  let g1 = create_num_card  "green" 1 in
  let g2 = create_num_card  "green" 2 in
  let g3 = create_num_card  "green" 3 in
  let b4 = create_num_card  "blue" 4 in
  let b5 = create_num_card  "blue" 5 in
  let b6 = create_num_card  "blue" 6 in
  let player_deck = create_deck empty_deck [r1; r2] in
  let ai_deck = create_deck empty_deck [y1; y2] in
  let current_c = create_num_card "blue" 1 in
  let d_deck = create_deck empty_deck [g3; g2; g1] in
  let p_deck = create_deck empty_deck [b6;b5;b4] in
  let player_played = empty_deck in
  let ai_played = empty_deck in
  let lastp_action = "None" in
  let test_state = create_state current_c player_deck ai_deck d_deck p_deck 
      player_played ai_played lastp_action true in

  let post_player_put = create_state r1 (add_card r2 empty_deck) ai_deck d_deck 
      (add_card r1 p_deck) (add_card r1 player_played) ai_played "put" false in

  let post_ai_put = create_state y1 (add_card r2 empty_deck) 
      (add_card y2 empty_deck) d_deck (create_deck p_deck [r1;y1]) 
      (add_card r1 player_played) (add_card y1 ai_played) "put" true in

  let post_player_draw = create_state y1 (create_deck empty_deck [r2; g1])
      (add_card y2 empty_deck) (create_deck empty_deck [g3; g2]) 
      (create_deck p_deck [r1; y1]) (add_card r1 player_played) 
      (add_card y1 ai_played) "draw" false in

  let post_ai_draw = create_state y1 (create_deck empty_deck [r2; g1]) 
      (create_deck empty_deck [y2; g2]) (add_card g3 empty_deck) 
      (create_deck p_deck [r1; y1]) (add_card r1 player_played) 
      (add_card y1 ai_played) "draw" true in

  let empty_draw_deck = create_state y1 player_deck ai_deck empty_deck 
      (create_deck empty_deck [b6;y1]) player_played ai_played "put" true in

  let post_empty_draw_deck = create_state y1 (create_deck player_deck [b6]) ai_deck 
      empty_deck (add_card y1 empty_deck) player_played ai_played "draw" false in

  [
    test_get_current_card "Current Card: Blue 1" test_state ("1", "blue");

    test_get_players_hand "Player Hand: Red 1 2" test_state 
      [("1", "red"); ("2", "red")];

    test_get_ai_hand "AI Hand: Yellow 1 2" test_state 
      [("1", "yellow"); ("2", "yellow")];

    test_get_draw_deck "Draw: Green 2 3" test_state 
      [("1", "green"); ("2", "green"); ("3", "green")];

    test_has_won "Has Won: False" test_state false;

    test_get_turn "Turn: True" test_state true;

    test_put "Player Puts Red 1" r1 test_state "player" post_player_put;
    test_put "AI Puts Yellow 1" y1 post_player_put "ai" post_ai_put;
    make_exception_test "Player Puts Invalid" Invalid_Move
      (fun _ -> State.put r2 post_ai_put "player");

    test_draw "Player Draws Green 1" post_ai_put "player" post_player_draw;
    test_draw "AI Draws Green 1" post_player_draw "ai" post_ai_draw;
    test_draw "Player Draws from empty deck" empty_draw_deck 
      "player" post_empty_draw_deck;
  ]  

let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite