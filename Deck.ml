
type color =
  |Red
  |Yellow
  |Green
  |Blue
  |Wild

type power = 
  | Reverse
  | Skip
  | Draw_Two
  | Draw_Four
  | Wild
  | No_Power

type number_card = {number : int; color: color}
type power_card = {power : power; color: color}

type card = 
  |Num_Card of number_card
  |Power_Card of power_card


type t = card list

exception Invalid_Color of string

exception Invalid_Power of string

exception Malformed_Card

let color_of_string str = 
  match str with
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "blue" -> Blue
  | "wild" -> Wild
  | _ -> raise (Invalid_Color str)

let power_of_string str = 
  match str with
  | "reverse" -> Reverse
  | "skip" -> Skip 
  | "draw two" -> Draw_Two
  | "draw four" -> Draw_Four
  | "wild" -> Wild
  | _ -> raise (Invalid_Power str)

let type_to_string c = 
  match c with
  |Num_Card n -> "number card"
  |Power_Card p -> "power card"

let create_num_card col num : card = 
  try 
    Num_Card {number = num; color = (color_of_string col)}
  with
  | Invalid_Color malCol -> failwith (malCol ^ " is not a valid color.")

let create_pow_card col pow : card =
  try
    Power_Card {power = (power_of_string pow); color = (color_of_string col)}
  with
  | Invalid_Color malCol -> failwith (malCol ^ " is not a valid color.")
  | Invalid_Power malPow -> failwith (malPow ^ " is not a valid power.")


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
  (load_num_color numbers "yellow") @ 
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

let string_of_power p =
  match p with
  |Draw_Four -> "draw four"
  |Draw_Two -> "draw two"
  |Skip ->  "skip"
  |Reverse -> "reverse"
  |Wild -> "wild"
  |No_Power -> "no power"

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

let remove_card (c:card) (d:t) :t = 
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

let is_powercard = function
  | Power_Card p -> true
  | _ -> false

let card_val (c:card) = 
  match c with
  | Num_Card n -> (n.number, No_Power)
  | Power_Card p -> (-1, p.power)


let string_of_color (c:color) = 
  match c with
  |Red -> "red"
  |Yellow -> "yellow"
  |Green -> "green"
  |Blue -> "blue"
  |Wild -> "wild"

let val_to_string c = 
  match (card_val c) with
  |(num, No_Power ) -> string_of_int num
  |(-1, pow) -> string_of_power pow 
  | _ -> failwith "invalid card"

let card_col (c:card) =
  match c with
  |Power_Card p -> string_of_color p.color
  |Num_Card n -> string_of_color n.color

let list_card (c:card) = 
  match c with
  |Power_Card p -> (string_of_power (p.power), card_col c)
  |Num_Card n -> (string_of_int (n.number), card_col c)


let to_list t = t |> List.map list_card

let card_equals c1 c2 =
  (val_to_string h) = val_to_string c && (card_col h) = (card_col c)

let rec deck_contains (c:card) (d:t) = 
  match d with
  | [] -> false
  | h::t when (card_equals c h) -> true
  | h::t -> deck_contains c t  

let get_valid_card c (d:t) = 
  let rec valid' c2 = function 
    |[]-> None
    |h::t -> if is_valid h c then Some h else valid' c2 t

  in valid' c d

let merge_decks (d1:t) (d2:t) = d1@d2

let get_power p = 
  match p with 
  |Power_Card c -> c.power
  |_ -> failwith "Not a Power Card"


let change_wild_color c col = 
  match c with 
  |Power_Card p -> Power_Card{power = p.power; color = (color_of_string col)}
  |_ -> c

let random_color = let () = Random.self_init() in
  match Random.int 4 with
  |0 -> Red
  |1 -> Blue
  |2 -> Green
  |3 -> Yellow
  |_ -> failwith "Impossible"

(** MEDIUM AI *)

let majority_color (d:t) = 
  let rec count_color acc c (dk:t) = 
    match dk with
    | [] -> acc
    | Power_Card p::t -> if p.color = c then count_color (acc+1) c t else count_color acc c t
    | Num_Card n :: t -> if n.color = c then count_color (acc+1) c t else count_color acc c t
  in 
  let r = count_color 0 Red d in 
  let y = count_color 0 Yellow d in 
  let g = count_color 0 Green d in 
  let b = count_color 0 Blue d in 
  let max1 = if r > y then Red else Yellow in 
  let max2 = if g > (max r y ) then Green else max1 in 
  if b >( max g (max r y)) then Blue else max2

let get_medium_card c (d:t)  = 
  let rec remove_wilds acc (dk:t) : t = 
    match dk with 
    | [] -> acc
    | Power_Card p::t -> if p.color = Wild then remove_wilds acc t 
                        else remove_wilds (Power_Card p::acc) t
    | Num_Card n :: t -> remove_wilds acc t 
  in 
  
  let rec remove_powers acc (dk:t) : t = 
    match dk with 
    | [] -> acc
    | Power_Card p::t -> remove_powers acc t
    | Num_Card n ::t -> remove_powers (Num_Card n::acc) t
  in 

  let wildless = remove_wilds [] d in
  match wildless with
  | [] -> get_valid_card c d
  | _ ->

  let powerless = remove_powers [] wildless in 
  match powerless with 
  | [] -> get_valid_card c wildless
  | _ -> get_valid_card c powerless

(* Helper function for [get_supreme_card]. *)
(** [top_consecutive_color d] is a tuple containing the color of the top card
    of deck [d] and an integer representing the number of consecutive cards of 
    that color following the top card. If the color of the top card is wild,
    the streak is 0. *)
let top_consecutive_color (d:t) = 
  let rec consecutive_helper col acc dk =
  match dk with
    | [] -> acc
    | h::t when (card_col h) <> col -> acc
    | h::t when (card_col h) = Wild -> acc + 1
    | h::t -> consecutive_helper col (acc+1) t
  in

  let tc = top_card d in
  let tcol = card_col tc in
  match tcol with
  |Wild -> (tcol, 0)
  |_ -> (tcol, (consecutive_helper tcol 0 d))


let get_supreme_card c (ai_hand:t) (p_hand:t) (g_played:t) 
                    (p_played:t) (ai_played:t) =
  
  if len (p_hand) = 1 then begin
  try
    let best_card = List.find 
        (fun cc ->  match cc with 
        |Power_Card p when p.power = Skip && (is_valid cc c) -> true
        |Power_Card p when p.power = Reverse && (is_valid cc c) -> true
        | _ -> false) d in
    Some (best_card, (d |> top_card |> card_col))
  with 
  | _ -> begin
  try 
    let best_card = List.find
        (fun cc -> match cc with
        |Power_Card p when p.power Draw_Two && (is_valid cc c) -> true)
        |_ -> false) d in
    Some (best_card, (d |> top_card |> card_col))
  with
  |_ -> begin
  try
    let best_card = List.find
      (fun cc -> match cc with
      |Power_Card p when p.power Draw_Four && (is_valid cc c) -> true)
  end
  end
  end

