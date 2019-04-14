open Deck
let deck_tests =

  let initial_deck = load_deck in 
  let my_deck = fst (deal initial_deck) in
  let ai_deck = fst deal (snd (deal initial_deck)) in 
  let remaining = snd deal (snd (deal initial_deck)) in 
  [
    (* Top Card tests **)
    let c = {color= Red; number = 0} in 
    let c2 = {color= Yellow; number = 6} in 
    "My Top Card " >:: (fun _ -> assert_equal c
                           (top_card my_deck));
    "Top Card Remaining" >:: (fun _ -> assert_equal c2
                                 (top_card my_deck));
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