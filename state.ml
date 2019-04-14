open Deck
type state = {
  current_card : card;
  players_hand : Deck.t;
  ai_hand : Deck.t;
  draw_deck : Deck.t ;

} 

let init_state d = failwith "Unimplemented"
let get_current_card st = failwith "Unimplemented"
let get_players_hand st = failwith "Unimplemented"
let get_ai_hand st = failwith "Unimplemented"
let get_draw_deck st = failwith "Unimplemented"