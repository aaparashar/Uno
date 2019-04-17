
type color =
  |Red
  |Yellow
  |Green
  |Blue

type card = {number : int; color: color}

type t = card list

let create_card (color:string) (num:int) = 
  match color with
  | "red" -> {number = num; color = Red}
  | "yellow" -> {number = num; color = Yellow}
  | "green" -> {number = num; color = Green}
  | "blue" -> {number = num; color = Blue}
  | _ -> failwith "invalid card"
(**[load_num_color numbers col] loads cards with numbers from numbers 
   and color col*)
let rec load_num_color (numbers:int list) col =
  let load_helper acc h = ({number = h; color = col}::acc) in
  List.fold_left load_helper [] numbers

let empty_deck: t = []
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

let len d = List.length d

let card_num c = c.number

let card_col c =
  match c.color with
  |Red -> "red"
  |Yellow -> "yellow"
  |Green -> "green"
  |Blue -> "blue"

let list_card c = (c.number, card_col c)

let to_list t = t |> List.map list_card

let contains (c:card) (d:t) = List.mem c d

let get_valid_card c (d:t) = 
  let rec valid' c2 = function 
    |[]-> None
    |h::t -> if is_valid h c then Some h else valid' c2 t

  in valid' c d