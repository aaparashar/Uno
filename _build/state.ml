open Deck
type t = {
  current_card : Deck.card;
  players_hand : Deck.t;
  ai_hand : Deck.t;
  draw_deck : Deck.t ;
  playing_deck : Deck.t ;
  turn:bool;
} 

exception Invalid_Move
let init_state  = 
  let deck = shuffle(load_deck) in 
  let players = fst (deal deck) in 
  let ai_deck = fst (deal (snd (deal deck))) in 
  let remaining = snd (deal (snd (deal deck))) in 
  let playing = add_card (top_card remaining) empty_deck in
  let remaining2 = remove_card (top_card remaining) remaining in
  {current_card = top_card playing; players_hand = players; ai_hand= ai_deck; 
   draw_deck=remaining2; playing_deck= playing; turn = true}

let get_current_card (st:t) = st.current_card
let get_players_hand st = st.players_hand
let get_ai_hand st = st.ai_hand
let get_draw_deck st = st.draw_deck
let has_won st = Deck.len st.players_hand = 0 || Deck.len st.ai_hand = 0
let get_current_score = None
let get_turn st = st.turn

let put c (st:t) s = if (is_valid c st.current_card && s="player" && contains c st.players_hand) 
  then {current_card = c;
        players_hand = remove_card c st.players_hand; 
        ai_hand= st.ai_hand; 
        draw_deck=st.draw_deck; 
        playing_deck= add_card c st.playing_deck; turn = false} 
  else if (is_valid c st.current_card && s="ai"&& contains c st.ai_hand)
  then {current_card = c;
        players_hand = st.players_hand;
        ai_hand= remove_card c st.ai_hand; 
        draw_deck=st.draw_deck; 
        playing_deck= add_card c st.playing_deck; turn = true;} 
  else raise Invalid_Move



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

let ai_turn st = 
  match (get_valid_card st.current_card st.ai_hand) with
  |None -> draw st "ai"
  |Some x -> put x st "ai"


