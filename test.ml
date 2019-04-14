open OUnit2
open Deck

(** [pp_card c] pretty-prints card [c]. *)
let pp_card (c:card) = 
"\""^(string_of_int c.number) ^", " ^color^"\""

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

let test_top_card 
    (name : string)
    (d: Deck.t)
    (expected : card) =
  name >:: (fun _ ->
    assert_equal expected (top_card d) ~printer:pp_card)

let test_add_card 
    (name : string)
    (d: Deck.t)
    (expected : Deck.T) =
  name >:: (fun _ ->
    assert_equal expected (add_card d) ~printer:(pp_deck pp_card))

let test_remove_card 
    (name : string)
    (d: Deck.t)
    (expected : Deck.T) =
  name >:: (fun _ ->
    assert_equal expected (remove_card d) ~printer:(pp_deck pp_card))

let test_is_valid 
    (name : string)
    (c1: card)
    (c2 : card)
    (expected : bool) =
  name >:: (fun _ ->
    assert_equal expected (is_valid c1 c2) ~printer:string_of_bool)

let deck_tests =
  let initial_deck = load_deck 
  let my_deck = fst (deal initial_deck)
  let ai_deck = fst deal (snd (deal initial_deck)) 
  let remaining = snd deal (snd (deal initial_deck)) 
  [
    (* Top Card tests **)
    let c1 = {color= Red; number = 0} in 
    let c2 = {color = Yellow; number = 3}
    let c3 = {color= Yellow; number = 6} in
    test_top_card "Player's Top Card" my_deck c1;
    test_top_card "Remaining Top Card" remaining c3;
  ]

let command_tests =
  [

  ]
let state_tests =
  [

  ]
let suite =
  "test suite for A6"  >::: List.flatten [
    deck_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite