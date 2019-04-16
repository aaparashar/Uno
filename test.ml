open OUnit2
open Deck
open Command
open State


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
  | Put phrase -> pp_list pp_string phrase
  | Draw -> pp_string "Draw"
  | Quit -> pp_string "Quit"
  | Score -> pp_string "Score"
  | Hand -> pp_string "Hand"
  | Play -> pp_string "Play"
  | _ -> pp_string "unimplemented command"

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


let test_shuffle
    (name : string)
    (d: Deck.t) = 
  let shuffled = shuffle d in
  name >:: (fun _ ->
      let dl = Deck.to_list d in
      let sl = Deck.to_list shuffled in
      assert_equal sl dl 
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

let deck_tests =
let initial_deck = load_deck in
let my_deck = fst (deal initial_deck) in
let ai_deck = fst (deal (snd (deal initial_deck))) in
let remaining = snd (deal (snd (deal initial_deck))) in
    [
      (* Top Card tests **)
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
  (let expected_cmd = Quit in
   make_parse_test "Parse: Quit" "Quit" expected_cmd);
    (* "Quit" >:: (fun _ -> assert_equal (parse "quit") Quit);
    "Draw" >:: (fun _ -> assert_equal (parse "draw") Draw);
    "Score" >:: (fun _ -> assert_equal (parse "score") Score);
    "Hand" >:: (fun _ -> assert_equal (parse "hand") Hand);
    "Play" >:: (fun _ -> assert_equal (parse "play") Play);
    "Put yellow 3" >:: (fun _ ->assert_equal Put ["yellow","3"] 
    (parse "put yellow 3"));  
    "Put red 8" >:: (fun _ -> assert_equal Put ["red","8"] 
    (parse "  put  red    8"));  
    "Put with empty" >:: (fun _ ->assert_equal Empty 
    (parse "put"));   *)



(* "Lock door" >:: (fun _ -> assert_equal (Command.Lock ["dorm with key"]) 
    (parse "lock dorm with key"));
   "Unlock door" >:: (fun _ -> assert_equal (Command.Unlock ["treasure with 
    book"]) (parse "unlock treasure with book")); *)
(* "Empty String" >:: (fun _ -> assert_raises (Empty) parse "");
"Malformed" >:: (fun _ -> assert_raises (Malformed) parse("potato")); *)
]

let state_tests =
  (* let c = {color =Yellow; number = 1} in
  let test = put c init_state "player" in  *)
  [
    (* "Current Card" >:: (fun _ ->assert_equal (get_current_card test) c );
    "Players Hand" >:: (fun _ ->assert_equal (get_players_hand test)  
                           (remove_card c (get_players_hand init_state) )); 
    "AI Hand" >:: (fun _ ->assert_equal (get_ai_hand test) 
                      (get_ai_hand init_state)) ; 
    "Draw Deck" >:: (fun _ ->assert_equal (get_draw_deck test)
                        (get_draw_deck init_state)); 
    "Playing Deck" >:: (fun _ ->assert_equal (get_playing_deck test) 
                           (add_card c (get_playing_deck init_state) ));  *)
  ]
let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite