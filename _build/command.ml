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
    let strarray = str |> String.split_on_char ' ' |> List.filter ((<>) "") in
    match strarray with
    |[] -> raise Empty
<<<<<<< HEAD
    |h::t when h = "Quit" -> Quit 
    |h::t when h = "Draw" -> Draw
   (* |h::t when h = "Score" -> Score*)
    |h::t when h = "Hand" -> Hand
=======
    |h::[] when h = "Quit" -> Quit 
    |h::[] when h = "Draw" -> Draw
    |h::[] when h = "Hand" -> Hand
>>>>>>> 1024233ca70ee1c15fdfabc63b29e6221341516e
    |h::t when h = "Put" && t <> [] -> Put t
    |_ -> raise Malformed
  end

