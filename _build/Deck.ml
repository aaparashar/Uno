
type color =
  |Red
  |Yellow
  |Green
  |Blue

type card = {number : int; color: color}

type t = card list

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

let rec riffle 
    (d:t) 
    (acc_a:t)
    (acc_b:t) = 
  match d with
  | [] -> List.rev acc_a @ List.rev acc_b
  | h::t -> let n = Random.int 2 in 
    if n = 1 then riffle t (acc_a@[h]) acc_b 
    else riffle t acc_a (acc_b@[h]) 

let rec multi_riffle 
    (d: card list) 
    (k:int) = 
  match k with
  |  0 -> d
  | _ -> multi_riffle (riffle d [] []) (k-1)

let shuffle d = let n = Random.int 8 in 
  match n with 
  | 0 -> multi_riffle d 1
  | _ -> multi_riffle d n

let deal d = 
  let rec deal' n d2 acc =
    match d2 with 
    |[]-> failwith "not enough cards in deck"
    |h::t -> if n=0 then (acc, t) else deal' (n-1) t (h::acc)
  in deal' 7 d []

let add_card (c:card) (d:t) :t = c::d
let remove_card (c:card) (d:t) :t= 
  let rec remove' (c2:card) d2 acc = 
    match d2 with
    |[] -> d
    |h::t -> if c2= h then acc@t else remove' c t (h::acc)
  in remove' c d []
let top_card d = 
  match d with 
  |[] -> failwith "No Cards in Deck"
  |h::t -> h
let is_valid  card1 card2 = 
  card1.color = card2.color ||card1.number=card2.number