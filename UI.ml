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
  match d with 
  | empty_deck  -> "\n"
  | _ -> let c = Deck.top_card d in 
        "\t"^(print_card c)^"\n" ^ print_hand (Deck.remove_card c d)

let rec do_play_game (st: State.t) =
  if State.has_won st then 
    begin
      ANSITerminal.(print_string [cyan]("\n You "));
      if Deck.len(State.get_players_hand st) = 0 then 
        ANSITerminal.(print_string [cyan]("Win :D Thanks for playing!"))
      else ANSITerminal.(print_string [cyan]("Lose :( Better luck next time")) 
    end
  else if State.get_turn st then begin
    ANSITerminal.(print_string [cyan]("\nLast Card:\t"));
    ANSITerminal.(print_string [white] (print_card(State.get_current_card st)));
    ANSITerminal.(print_string [cyan]("\n In Your Hand:\n"));
    ANSITerminal.(print_string[white](print_hand(State.get_players_hand st)));
    if Deck.len(State.get_ai_hand st) = 1 then 
      ANSITerminal.(print_string [cyan]("\n AI says:\t"^"\"UNO\"")); 
    ANSITerminal.(print_string [cyan] ("\nWhat's your next move?\n"));
    ANSITerminal.(print_string [white] "> ");
    let (out_string,cmd) = 
      match Command.parse (read_line()) with
      | exception (Command.Empty) -> ("\nWhat's your next move?\n","");
      | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","");
      | Quit -> (("\n\nSee ya next time!\n"),"quit")
      | Hand -> ("\n In Your Hand:\n"^print_hand(State.get_players_hand st),"")
      | Draw -> ("\n You drew a card.","draw")
      | Put t-> 
         (match t with
          | num::col::[] -> (num^","^col, "put")
          | _ -> ("", "put") )
    in
    match cmd with
    | "put" -> 
        (let is_card n c = n >= 0 && n <= 9 && 
                          List.mem c ["red"; "yellow"; "blue"; "green"]
        in
        let card_comp = String.split_on_char ',' out_string in
        if List.length card_comp = 2 && 
        is_card (int_of_string (List.nth card_comp 0)) (List.nth card_comp 1) then
        begin
            let num = int_of_string (List.nth card_comp 0) in 
            let col = List.nth card_comp 1 in 
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
    | "draw" -> do_play_game (State.draw st "player")
    | "quit" -> let () = ANSITerminal.(print_string[cyan] out_string)in  exit 0;
    | _ -> ANSITerminal.(print_string[cyan] out_string);
      do_play_game st
  end
  else do_play_game (State.ai_turn st)



let play_game p = 
  ANSITerminal.(print_string [cyan] ("Welcome, "^p^" to the Uno Casino."));
  ANSITerminal.(print_string [cyan] "\nLet's deal you in");  
  do_play_game (State.init_state)



let rec main () = 
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to the Uno Game Engine\n"); 
  print_endline "Please enter your player name:\n";
  ANSITerminal.(print_string [white]  "> ");
  match read_line () with
  | "" -> main ()
  | pname -> play_game pname 