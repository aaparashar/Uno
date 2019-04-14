open Deck
open Command
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
    "Quit" >:: (fun _ -> assert_equal (parse "quit") Quit);
    "Draw" >:: (fun _ -> assert_equal (parse "draw") Draw);
    "Score" >:: (fun _ -> assert_equal (parse "score") Score);
    "Hand" >:: (fun _ -> assert_equal (parse "hand") Hand);
    "Play" >:: (fun _ -> assert_equal (parse "play") Play);
    "Put" >:: (fun _ ->assert_equal Put["yellow","3"]) 
      (parse "p yellow 3"));  
"Put with spaces" >:: (fun _ ->assert_equal Put["yellow","3"]) 
  (parse "draw yellow      3"));  
"Put with empty" >:: (fun _ ->assert_equal Empty) 
  (parse "put"));  



(* "Lock door" >:: (fun _ -> assert_equal (Command.Lock ["dorm with key"]) 
    (parse "lock dorm with key"));
   "Unlock door" >:: (fun _ -> assert_equal (Command.Unlock ["treasure with 
    book"]) (parse "unlock treasure with book")); *)
"Empty String" >:: (fun _ -> assert_raises (Empty) parse "");
"Malformed" >:: (fun _ -> assert_raises (Malformed) parse("potato"));
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