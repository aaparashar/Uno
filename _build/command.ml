type object_phrase = string list

type command = 
  | Draw
  | Quit
  | Score
  | Hand
  | Play 
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
    |h::t when h = "quit" -> Quit 
    |h::t when h = "draw" -> Draw
    |h::t when h = "score" -> Score
    |h::t when h = "hand" -> Hand
    |h::t when h = "play" -> Play
    |h::t when h = "put" && t <> [] -> Put t
    |_ ->  raise Malformed
  end

