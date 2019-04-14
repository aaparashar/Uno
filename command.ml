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
  else let strarray = String.split_on_char ' ' str in
    match strarray with
    |[] -> raise Empty
    |h::t -> if h = "quit" then Quit else if 
        h = "draw" then Draw
      else if h="score" then Score
      else if h="hand" then Hand
      else if h ="play" then Play
      else if h = "put" then Put(t)
      else raise Malformed

