
type color =
  |Red
  |Yellow
  |Green
  |Blue
  |Wild
type number_card = {number : int; color: color}
type power_card = {power : string; color: color}

type card = 
  |Num_Card of number_card
  |Power_Card of power_card


type t = card list

let create_num_card col num : card = 
 match col with
  | "red" -> Num_Card {number = num; color = Red}
  | "yellow" -> Num_Card {number = num; color = Yellow}
  | "green" -> Num_Card {number = num; color = Green}
  | "blue" -> Num_Card {number = num; color = Blue}
  | _ -> failwith "invalid card"

let create_pow_card col pow : card =
  match col with
  | "red" -> Power_Card {power = pow; color = Red}
  | "yellow" -> Power_Card {power = pow; color = Yellow}
  | "green" -> Power_Card {power = pow;  color = Green}
  | "blue" -> Power_Card {power = pow;  color = Blue}
  | "wild" -> Power_Card {power = pow; color = Wild}
  | _ -> failwith "invalid card"

(*let create_card c =
  match c with 
  | number_card -> Num_Card c 
  | power_card -> Power_Card c*)

let rec load_num_color (numbers:int list) col : t =
  let load_helper acc h = (create_num_card col h::acc) in
  List.fold_left load_helper [] numbers

let rec load_pow_color (powers: string list) col : t = 
  let load_helper acc h = (create_pow_card col h:: acc) in 
  List.fold_left load_helper [] powers

let empty_deck: t = []

let load_deck = 
  let numbers = [0;1;2;3;4;5;6;7;8;9] in 
  let no_zero = [1;2;3;4;5;6;7;8;9] in
  let powers = ["skip"; "reverse"; "draw two"] in
  let wild_powers = ["wild"; "draw four"] in

  (load_num_color numbers "red") @
  (load_num_color numbers "yellow" @ 
  (load_num_color numbers "green") @
  (load_num_color numbers "blue" ) @ 
  (load_num_color no_zero "red") @
  (load_num_color no_zero "yellow") @ 
  (load_num_color no_zero "green") @
  (load_num_color no_zero "blue" ) @
  (load_pow_color powers "red") @
  (load_pow_color powers "yellow") @
  (load_pow_color powers "green") @
  (load_pow_color powers "blue" ) @
  (load_pow_color powers "red") @
  (load_pow_color powers "yellow") @
  (load_pow_color powers "green") @
  (load_pow_color powers "blue" ) @
  (load_pow_color wild_powers "wild")
  

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

let shuffle d = Random.self_init (); let n = Random.int 16 in 
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
  match (card1,card2) with
  |(Num_Card n1,Num_Card n2) -> n1.color= n2.color || n1.number = n2.number
  |(Power_Card p1, Power_Card p2) -> p1.power=p2.power ||p1.color=p2.color
  |(Num_Card n1,Power_Card p1) -> n1.color= p1.color 
  |(Power_Card p1, Num_Card n1) -> n1.color= p1.color 

let len d = List.length d

let card_num (c:number_card) = c.number

(**[color_to_string c] is the string form of color c *)
let color_to_string (c:color) = 
  match c with
  |Red -> "red"
  |Yellow -> "yellow"
  |Green -> "green"
  |Blue -> "blue"
  |Wild -> "wild"

let card_col (c:card) =
  match c with
  |Power_Card p -> color_to_string p.color
  |Num_Card n -> color_to_string n.color

let list_card (c:card) = 
  match c with
  |Power_Card p -> (p.power, card_col c)
  |Num_Card n -> (string_of_int (n.number), card_col c)


let to_list t = t |> List.map list_card

let deck_contains (c:card) (d:t) = List.mem c d

let get_valid_card c (d:t) = 
  let rec valid' c2 = function 
    |[]-> None
    |h::t -> if is_valid h c then Some h else valid' c2 t

  in valid' c d