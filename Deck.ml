
type color =
  |Red
  |Yellow
  |Green
  |Blue
type card = {number : int; color: color}
type deck = card list
let load_deck = failwith "Unimplemented"
let shuffle d = failwith "Unimplemented"
let deal d = failwith "Unimplemented"
let add_card c d = c::d
let remove_card c d = 
  match d with
  |[] -> d
  |h::t -> if c= h then failwith "Unimplemented" else failwith "Unimplemented"
let top_card d = 
  match d with 
  |[] -> failwith "No Cards in Deck"
  |h::t -> h
let is_valid  card1 card2 = 
  card1.color = card2.color ||card1.number=card2.number