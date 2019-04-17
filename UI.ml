open Deck
open State
open ANSITerminal
open Format


let pp_card (c:card) = 
  ANSITerminal.(print_string [Deck.card_col c] (Deck.card_col c) ^" "^string_of_int(Deck.card_num c))

let rec print_hand d =
  match d with 
  | h::t -> "\t"^(print_card c)^"\n" ^ print_hand t
  | [] -> "\n"
(** string version*)
let print_card c =
  (Deck.card_col c )^" " ^ (Deck.card_num c)

let rec do_play_game st =
  if State.has_won st then 
    begin
      ANSITerminal.(print_string [cyan]("\n You "));
      if st.players_hand = 0 then ANSITerminal.(print_string [cyan]("Win :D Thanks for playing!"))
      else ANSITerminal.(print_string [cyan]("Lose :( Better luck next time")) 
    end
  else if st.turn then begin
    ANSITerminal.(print_string [cyan]("\nLast Card:\t"));
    print_string (print_card(st.current_card));
    ANSITerminal.(print_string [cyan]("\n In Your Hand:\n"));
    print_string(print_hand(st.players_hand));
    if Deck.len(st.get_ai_hand) = 1 then ANSITerminal.(print_string [cyan]("\n AI says:\t \"UNO\""));
    ANSITerminal.(print_string [cyan] ("\nWhat's your next move?\n"));
    print_string  "> ";
    let (out_string,cmd) = 
      match Command.parse (read_line()) with
      | exception (Command.Empty) -> ("\nWhat's your next move?\n","");
      | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","");
      | Quit -> (("\n\nSee ya next time!\n"),"quit")
      | Score -> ("Score:\t"^(string_of_int(State.get_current_score st))^"\n", "")
      | Hand -> ("\n In Your Hand:\n"^print_hand(st.players_hand),"")
      | Put(t)-> ("\n You played:\t"^print_card(t),"put")
      | Draw -> ("\n You drew a card.","draw")
    in
    match cmd with
    | "put" -> 
      (match State.put t st "player" with
       | Invalid_Move -> ANSITerminal(print_string[magenta]"\n Can't play that card \n");
         do_play_game st
       | newState -> ANSITerminal(print_string[cyan] out_string); 
         do_play_game newState)
    | "draw" -> State.draw st "player"
    | "quit" -> let () = ANSITerminal(print_string[cyan] out_string)in  exit 0;
    | _ -> ANSITerminal(print_string[cyan] out_string);
      do_play_game st
  end
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