type object_phrase = string list

type command = 
  | Draw
  | Quit
  | Hand
  | Put of object_phrase


exception Empty

exception Malformed

(** [parse str] takes string [str] and splits it at any space characters, 
    returning empty for the empty string, a command of value Quit if the user 
    typed quit, or a Play command with value equal to a list of all strings 
    following the first if the first string is go**)
let parse str =
  if str = "" then raise Empty
  else begin 
    let strarray = String.split_on_char ' ' str in
    match strarray with
    |[] -> raise Empty
    |h::[] when h = "Quit" -> Quit 
    |h::[] when h = "Draw" -> Draw
    |h::[] when h = "Score" -> Score
    |h::[] when h = "Hand" -> Hand
    |h::t when h = "Put" && t <> [] -> Put t
    |_ -> raise Malformed
  end

