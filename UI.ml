open Deck
open State
open Command
open ANSITerminal

let style_color color = 
  match color with 
  | "red" -> [red]
  | "yellow" -> [yellow]
  | "green" -> [green]
  | "blue" -> [blue]
  | _ -> failwith "unimplemented color"

let pp_card (c:card) = 
  let sty = style_color (Deck.card_col c) in
  ANSITerminal.(print_string sty ((Deck.card_col c)^" "^string_of_int(Deck.card_num c)))

(* string version*)
let print_card c =
  (Deck.card_col c )^" "^(string_of_int (Deck.card_num c))

let rec print_hand (d:Deck.t) =
  if Deck.len d = 0 then ANSITerminal.(print_string[white]"\n")
  else let c = Deck.top_card d in 
    ANSITerminal.(print_string [white]("\t")); 
    pp_card c;
    ANSITerminal.(print_string [white]("\n"));
    print_hand (Deck.remove_card c d)

let rec do_play_game (st: State.t) =
  if State.has_won st then 
    begin
      ANSITerminal.(print_string [cyan]("\n You "));
      if Deck.len(State.get_players_hand st) = 0 then 
        let () = ANSITerminal.(print_string [cyan]("Win :D Thanks for playing!")) 
        in  exit 0;
      else let () = 
             ANSITerminal.(print_string [cyan]("Lose :( Better luck next time")) 
        in exit 0;
    end
  else if State.get_turn st then begin
    ANSITerminal.(print_string [cyan]("\nLast Card:\t"));
    ANSITerminal.(print_string [white] (print_card(State.get_current_card st)));
    ANSITerminal.(print_string [cyan]("\n In Your Hand:\n"));
    print_hand(State.get_players_hand st);
    if Deck.len(State.get_ai_hand st) = 1 then 
      ANSITerminal.(print_string [cyan]("\n AI says:\t"^"\"UNO\"")); 
    ANSITerminal.(print_string [cyan] ("\nWhat's your next move?\n"));
    ANSITerminal.(print_string [white] "> ");
    let (out_string,cmd) = 
      match Command.parse (read_line()) with
      | exception (Command.Empty) -> ("\nWhat's your next move?\n","");
      | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","");
      | Quit -> (("\n\nSee ya next time!\n"),"Quit")
      | Hand -> ("\n In Your Hand:\n","Hand")
      | Draw -> ("\n You drew a card.","Draw")
      | Put t-> 
        (match t with
         | num::col::[] -> (num^","^col, "Put")
         | _ -> ("", "Put") )
    in
    match cmd with
    | "Put" -> 
      (let is_card n c = n >= 0 && n <= 9 && 
                         List.mem c ["red"; "yellow"; "blue"; "green"]
       in
       let card_comp = String.split_on_char ',' out_string in
       if List.length card_comp = 2 && 
          is_card (int_of_string (List.nth card_comp 1)) (List.nth card_comp 0) then
         begin
           let num = int_of_string (List.nth card_comp 1) in 
           let col = List.nth card_comp 0 in 
           let c = Deck.create_card col num in
           match State.put c st "player" with
           | newState -> 
             (ANSITerminal.(print_string [cyan]("\nYou played:\t"^print_card c));
              do_play_game newState)
           | exception(Invalid_Move) -> 
             (ANSITerminal.(print_string [magenta]"\n Can't put that there\n");
              do_play_game st )
         end
       else 
         begin
           ANSITerminal.(print_string [magenta]"\n That's not a card\n");
           do_play_game st
         end
      )
    | "Draw" -> do_play_game (State.draw st "player")
    | "Quit" -> let () = ANSITerminal.(print_string[cyan] out_string) in  exit 0;
    | "Hand" -> ANSITerminal.(print_string[cyan] out_string);
      print_hand(State.get_players_hand st);
      do_play_game st;
    | _ -> ANSITerminal.(print_string[cyan] out_string);
      do_play_game st
  end
  else 
    ANSITerminal.(print_string[white] "\nAI is playing\n");
  do_play_game (State.ai_turn st)



let play_game p = 
  ANSITerminal.(print_string [cyan] ("Welcome, "^p^" to the Uno Casino."));
  ANSITerminal.(print_string [cyan] "\nLet's deal you in");  
  do_play_game (State.init_state)



let rec main () = 
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to the Uno Game Engine\n"); 
  print_endline 
    "Reminder that all commands start with an uppercase letter and a card is 
  represented as lowercase with a color then a number i.e. Put blue 8.\n";
  print_endline "Please enter your player name:\n";
  ANSITerminal.(print_string [white]  "> ");
  match read_line () with
  | "" -> main ()
  | pname -> play_game pname 

let () = main ()