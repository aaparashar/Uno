open Deck
type t = {
  current_card : Deck.card;
  players_hand : Deck.t;
  ai_hand : Deck.t;
  draw_deck : Deck.t ;
  playing_deck : Deck.t ;
  turn : bool;
} 

exception Invalid_Move

let create_state curr_c pl_h ai_h draw_d pl_d is_turn =
  {current_card = curr_c; 
   players_hand = pl_h; 
   ai_hand = ai_h;
   draw_deck = draw_d;
   playing_deck = pl_d;
   turn = is_turn}
(** [find_top c acc d ] checks if the top card c is a number card if it is not 
    it recurses through the rest of deck [d] untill it finds the firts number card*)
let find_top c acc d= 
  match c with 
  |Num_Card n -> (n, merge_decks d acc)
  |Power_Card p ->  check_top (top_card d) (add_card p acc) remove_card p d

let init_state  = 
  let deck = shuffle(load_deck) in 
  let players = fst (deal deck) in 
  let ai_deck = fst (deal (snd (deal deck))) in 
  let remaining = snd (deal (snd (deal deck))) in 
  let playing = add_card (top_card remaining) Deck.empty_deck in
  let remaining2 = remove_card (top_card remaining) remaining in
  let top = fst(find_top (top_card playing) empty_deck playing) in 
  let playing = snd (find_top (top_card playing) empty_deck playing) in 


  {current_card = top; players_hand = players; ai_hand= ai_deck; 
   draw_deck=remaining2; playing_deck= playing; turn = true}

let get_current_card (st:t) = st.current_card
let get_players_hand st = st.players_hand
let get_ai_hand st = st.ai_hand
let get_draw_deck st = st.draw_deck
let get_playing_deck st = st.playing_deck
let has_won st = Deck.len st.players_hand = 0 || Deck.len st.ai_hand = 0
(* let get_current_score = None *)
let get_turn st = st.turn

let draw (st:t) s = 
  if (st.draw_deck = empty_deck && s= "player") then 
    let reset = {st with draw_deck=shuffle (remove_card st.current_card st.playing_deck); 
                         playing_deck = add_card st.current_card empty_deck} in 
    {reset with players_hand = (add_card (top_card reset.draw_deck) reset.players_hand); 
                draw_deck = remove_card (top_card reset.draw_deck) reset.draw_deck; turn = false;}
  else if (st.draw_deck = empty_deck && s= "ai") then 
    let reset = {st with draw_deck=shuffle (remove_card st.current_card st.playing_deck); 
                         playing_deck = add_card st.current_card empty_deck} in 
    {reset with ai_hand = (add_card (top_card reset.draw_deck) reset.ai_hand); 
                draw_deck = remove_card (top_card reset.draw_deck) reset.draw_deck;turn=true;}
  else if (s = "ai") then 
    {st with ai_hand = (add_card (top_card st.draw_deck) st.ai_hand); 
             draw_deck = remove_card (top_card st.draw_deck) st.draw_deck;turn=true;}
  else 
    {st with players_hand = (add_card (top_card st.draw_deck) st.players_hand); 
             draw_deck = remove_card (top_card st.draw_deck) st.draw_deck;turn=false;}


let put c (st:t) s = 
  if ((is_valid st.current_card c )&& (s="player") && (deck_contains c st.players_hand)) 
  then 
    match c with 
    |Num_Card ->{st with current_card = c;
                         players_hand = remove_card c st.players_hand;  
                         playing_deck= add_card c st.playing_deck; 
                         turn = false} 
    |Power_Card p -> match p.power with
      |"draw two" -> draw (draw st "ai") "ai"
      |"draw four" -> draw (draw(draw (draw st "ai") "ai") "ai") "ai"
      |"skip" -> {st with current_card = c;
                          players_hand = remove_card c st.players_hand;  
                          playing_deck= add_card c st.playing_deck; 
                          turn = true} 
      |"reverse" ->{st with current_card = c;
                            players_hand = remove_card c st.players_hand;  
                            playing_deck= add_card c st.playing_deck; 
                            turn = true} 
      |"wild" -> failwith "Unimplemented"
      |_ -> raise Invalid_Move
  else if (is_valid c st.current_card && s="ai"&& deck_contains c st.ai_hand)
  then match c with 
    |Num_Card ->{st with current_card = c;
                         ai_hand = remove_card c st.ai_hand;  
                         playing_deck= add_card c st.playing_deck; 
                         turn = false} 
    |Power_Card p -> match p.power with
      |"draw two" -> draw (draw st "player") "player"
      |"draw four" -> draw (draw(draw (draw st "player") "player") "player") "player"
      |"skip" -> {st with current_card = c;
                          ai_hand = remove_card c st.ai_hand;  
                          playing_deck= add_card c st.playing_deck; 
                          turn = false} 
      |"reverse" ->{st with current_card = c;
                            ai_hand = remove_card c st.ai_hand;  
                            playing_deck= add_card c st.playing_deck; 
                            turn = false} 

      |"wild" -> failwith "Unimplemented"
      |_ -> raise Invalid_Move

  else raise Invalid_Move




let ai_turn st = 
  match (get_valid_card st.current_card st.ai_hand) with
  |None -> draw st "ai"
  |Some x -> put x st "ai"


