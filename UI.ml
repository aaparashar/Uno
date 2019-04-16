open Deck
open state
open ANSITerminal
open Format

let pp_card (c:card) = 
  ANSITerminal.(string_of_int[c.color] c.number)

let rec print_hand d =
  match d with 
  | h::t -> "\t"^pp_card^"\n" ^ print_hand t
  | [] -> "\n"
let print_card c =
  ANSITerminal.([c.color] (c.color+ " " c.number))

let rec do_play_game st =
  if State.has_won st then
    ANSITeriminal.(print_string [cyan]("\n You "));
  if st.players_hand = 0 then ANSITeriminal.(print_string [cyan]("Win :D Thanks for playing!"));
else ANSITeriminal.(print_string [cyan]("Lose :( Better luck next time"));
else if st.turn
    ANSITeriminal.(print_string [cyan]("\nLast Card:\t"));
  print_string (print_card(st.current_card));
  ANSITeriminal.(print_string [cyan]("\n In Your Hand:\n"));
  print_string(print_hand(st.players_hand));
  ANSITeriminal.(print_string [cyan] ("\nWhat's your next move?\n"));
  print_string  "> ";
  let (out_string,cmd) = 
    match Command.parse (read_line()) with
    | exception (Command.Empty) -> ("\nWhat's your next move?\n","");
    | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","");
    | Quit -> ("\n\nSee ya next time!\n")
                exit 0;
    | Score -> ("Score:\t"^(string_of_int(State.get_current_score st))^"\n", "")
    | Hand -> ("\n In Your Hand:\n"^print_hand(st.players_hand),"")
    | Put(t)-> ("\n You played:\t"^print_card(t),"put")
    | Draw -> ("\n You drew a card.")
  in
  match cmd with
  | "put" -> 
    (match State.put t st "player" with
     | Invalid_Move -> ANSITeriminal(print_string[magenta]"\n Can't play that card \n");
       do_play_game st
     | newState -> ANSITeriminal(print_string[cyan] out_string); 
       do_play_game newState)
  | "draw" -> State.draw st "player"
  | _ -> ANSITeriminal(print_stringpcyan out_string);
    do_play_game st

else st.ai_turn



let play_game p = 
  ANSITerminal.(print_string [cyan] "Welcome, "^pname^" to the Uno Casino.");
  ANSITerminal.(print_string [cyan] "\nLet's deal you in");  
  do_play_game (State.init_state)



let rec main () = 
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to the Uno Game Engine\n"); 
  print_endline "Please enter your player name:\n";
  print_string  "> ";
  match read_line () with
  | "" -> main ()
  | pname -> play_game pname 