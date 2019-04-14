
type color =
  |Red
  |Yellow
  |Green
  |Blue

type card = {number : int; color: color}

type deck = card list

let rec load_num_color (numbers:int list) col =
  match numbers with
  | [] -> []
  | h::t -> let c = {number = h; color = col} in 
    c ::load_num_color t col

let load_deck = 
  let numbers = [0;1;2;3;4;5;6;7;8;9] in 
  let no_zero = [1;2;3;4;5;6;7;8;9] in
  (load_num_color numbers Red) @
  (load_num_color numbers Yellow) @ 
  (load_num_color numbers Green) @
  (load_num_color numbers Blue) @ 
  (load_num_color no_zero Red) @
  (load_num_color no_zero Yellow) @ 
  (load_num_color no_zero Green) @
  (load_num_color no_zero Blue)

let shuffle d = failwith "Unimplemented"
let deal d = failwith "Unimplemented"
let add_card c d = c::d
let rec remove_card c d acc= 
  match d with
  |[] -> d
  |h::t -> if c= h then acc@t else remove_card c t (h::acc)
let top_card d = 
  match d with 
  |[] -> failwith "No Cards in Deck"
  |h::t -> h
let is_valid  card1 card2 = 
  card1.color = card2.color ||card1.number=card2.number