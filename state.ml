open Deck
type state = {
  current_card : card;
  players_hand : Deck.t;
  ai_hand : Deck.t;
  draw_deck : Deck.t ;
  playing_deck : Deck.t ;


} 

let init_state  (d:Deck.t) = 
  let deck = d.shuffle(load_deck) in 
  let players = fst (d.deal deck) in 
  let ai_deck = fst d.deal (snd (d.deal deck)) in 
  let remaining = snd d.deal (snd (d.deal deck)) in 
  let playing = d.top_card remaining ::[] in
  let remaining2 = d.remove_card (d.top_card remaining) remaining in
  {current_card = d.top_card playing; players_hand = players; ai_hand= ai_deck; 
   draw_deck=remaining2; playing_deck= playing}

let get_current_card st = st.current_card
let get_players_hand st = st.players_hand
let get_ai_hand st = st.ai_hand
let get_draw_deck st = st.draw_deck

let pick c st d= failwith "Unimplemented"
let put c st d = failwith "Unimplemented"