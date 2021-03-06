open Deck
open State
open Command
open ANSITerminal

(*[style_color color] returns the ANSI Terminal corresponding [color] to a card*) 
let style_color color = 
  match color with 
  | "red" -> [red]
  | "yellow" -> [yellow]
  | "green" -> [green]
  | "blue" -> [blue]
  | "wild" -> [magenta]
  | _ -> failwith "unimplemented color"

(*[pp_card c] pretty prints card [c]*) 
let pp_card (c:card) = 
  let sty = style_color (Deck.card_col c) in
  ANSITerminal.(print_string sty ((Deck.card_col c)^" "^Deck.val_to_string c))

(* [print_card c] prints the color and value of card [c]*)
let print_card c =
  (Deck.card_col c )^" "^(Deck.val_to_string c)


(* [generate_art str s] repeats the string [str] [s] times*)
let rec generate_art str s =
  match s with
  |1-> str 
  |x ->str ^ generate_art str (s-1) 

(* [card_art str c] makes ASCII card art for card [c]*)
let card_art c = 
  let sty = style_color (Deck.card_col c) in
  let value = match Deck.val_to_string c with 
    |"draw two" -> "+2"
    |"draw four" -> "+4"
    |"skip"->"S"
    |"reverse" -> "R"
    |x ->  x in 

  ANSITerminal.(print_string sty 
                  ("\n "^generate_art "-" ((String.length (value) +2))
                   ^"\n|" ^(generate_art " " ((String.length (value) +2)))^"|\n"^ 
                   "| "^ (value)^" |"
                   ^"\n|" ^(generate_art " " ((String.length (value) +2)))^"|\n "^ 
                   (generate_art "-" ((String.length (value) +2)))))



(* [print_hand d] prints all the ASCII art for cards*)
let rec print_hand (d:Deck.t) =
  if Deck.len d = 0 then ANSITerminal.(print_string[white]"\n")
  else let c = Deck.top_card d in 
    ANSITerminal.(print_string [white]("\t")); 
    card_art c; 
    print_hand (Deck.remove_card c d)
(* [do_play_gane st mode] facilitates the game in state [st] and maintains mode
   [mode]*)
let rec do_play_game (st: State.t) (mode:string) =
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
    card_art(State.get_current_card st);
    ANSITerminal.(print_string [cyan]("\n In Your Hand:\n"));
    print_hand(State.get_players_hand st);
    if Deck.len(State.get_ai_hand st) = 1 then 
      ANSITerminal.(print_string [cyan]("\n AI says:\t"^"\"UNO\"")); 
    ANSITerminal.(print_string [cyan] ("\nWhat's your next move?\n"));
    ANSITerminal.(print_string [white] "> ");
    let (out_string, col, cmd) = 
      match Command.parse (read_line()) with
      | exception (Command.Empty) -> ("\nWhat's your next move?\n","","");
      | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","","");
      | Quit -> (("\n\nSee ya next time!\n"),"","Quit")
      | Hand -> ("\n In Your Hand:\n","","Hand")
      | Draw -> ("\n You drew a card.","","Draw")
      | Put t-> 
        (match t with
         | col::v1::v2::[] -> (v1^" "^v2, col, "Put")
         | col::v::[] -> (v, col, "Put")
         | _ -> ("", "", "Put") )
    in
    match cmd with
    | "Put" -> 
      (let is_num_card n c = n >= 0 && n <= 9 && 
                             List.mem c ["red"; "yellow"; "blue"; "green"] in
       let is_pow_card p c = List.mem p ["reverse"; "skip"; "draw two"; "draw four"; "wild"] &&
                             List.mem c ["red"; "yellow"; "blue"; "green"; "wild"] in
       let is_card s c = 
         try 
           is_num_card (int_of_string s) c
         with Failure "int_of_string" ->
           is_pow_card s c
       in  
       if col <> "" && out_string <> "" && is_card out_string col then
         begin
           let cc = 
             try
               Deck.create_num_card col (int_of_string out_string) 
             with Failure "int_of_string" ->
               Deck.create_pow_card col out_string
           in
           match State.put cc st "player" with
           | newState -> 
             (ANSITerminal.(print_string [cyan]("\nYou played:\t"^print_card (State.get_current_card newState)));
              do_play_game newState mode)
           | exception(Invalid_Move) -> 
             (ANSITerminal.(print_string [magenta]"\n Can't put that there\n");
              do_play_game st mode)
         end
       else 
         begin
           ANSITerminal.(print_string [magenta]"\n That's not a card\n");
           do_play_game st mode
         end
      )
    | "Draw" -> do_play_game (State.draw st "player") mode
    | "Quit" -> let () = ANSITerminal.(print_string[cyan] out_string) in  exit 0;
    | "Hand" -> ANSITerminal.(print_string[cyan] out_string);
      print_hand(State.get_players_hand st);
      do_play_game st mode;
    | _ -> ANSITerminal.(print_string[cyan] out_string);
      do_play_game st mode
  end
  else begin
    ANSITerminal.(print_string[white] "\nAI is playing\n");
    let next_state = 
      if mode = "easy" then
        State.dumb_ai_turn st
      else if mode="medium"  then try State.medium_ai_turn st 
        with | Invalid_Color t -> failwith "AI made invalid color!"
             | Invalid_Power t -> failwith "AI made invalid power!"
             | Invalid_Move -> failwith "AI made invalid move!"
             | _ -> failwith "AI failed"
      else State.supreme_ai_turn st in
    if (State.get_current_card next_state) <> (State.get_current_card st) then begin
      ANSITerminal.(print_string [cyan]("\n AI played:\t"^(print_card (State.get_current_card next_state))^"\n"));
      card_art(State.get_current_card next_state); 
      do_play_game next_state mode  end
    else 
      ANSITerminal.(print_string [cyan]("\n AI drew a card\n")); 
    do_play_game next_state mode  
  end 


(** [play_game p m] begins game with player name [p] and mode [m]*)
let play_game p m = 
  ANSITerminal.(print_string [cyan] ("Welcome, "^p^", to "));

  ANSITerminal.(print_string [red]
                  ("\n.----------------. .-----------------..----------------.\n" ^
                   "| .--------------. | .--------------. | .--------------. |\n"^
                   "| | _____  _____ | | | ____  _____  | | |     ____     | |\n"^
                   "| ||_   _||_   _|| | ||_   \\|_   _| | | |   .'    `.   | |\n"^
                   "| |  | |    | |  | | |  |   \\ | |   | | |  /  .--.  \\  | |\n"^
                   "| |  | '    ' |  | | |  | |\\ \\| |   | | |  | |    | |  | |\n"^
                   "| |   \\ `--' /   | | | _| |_\\   |_  | | |  \  `--'  /   | |\n"^
                   "| |    `.__.'    | | ||_____|\\____| | | |   `.____.'   | |\n"^
                   "| |              | | |              | | |              | |\n"^
                   "| '--------------' | '--------------' | '--------------' |\n"^
                   "'----------------' '----------------' '----------------' \n"));

  ANSITerminal.(print_string [cyan] "\nLet's deal you in");  
  do_play_game (State.init_state) m


(** [main] collects player name and mode and excutes game*)
let rec main () = 
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to the Uno Game Engine\n"); 
  print_endline 
    "Reminder that all commands start with an uppercase letter. A card is  
represented as lowercase sequence of phrases: a color then a number or a 
power. The available commands are \"Put\", \"Draw\", \"Quit\" and \"Hand\". 
Example Put commands: \nPut blue 8\nPut wild wild\nPut wild draw four
Put red draw two\nPut yellow skip\n";
  print_endline "Please enter your player name:\n";
  ANSITerminal.(print_string [white]  "> ");
  let pname =read_line() in
  print_endline "\nPlease enter whether you would like easy medium 
      or hard mode:\n";
  ANSITerminal.(print_string [white]  "> ");
  match (pname, read_line ()) with
  | ("",_) -> print_endline "\nYou must enter something for 
  both your name and the level"; main ()
  |(_,"") -> print_endline "\nYou must enter something for 
  both your name and the level"; main ()
  | (name, mode) ->( if (mode="easy"||mode="hard"||mode="medium") then play_game pname mode
                     else 
                       print_endline ("Invalid mode try again!"^
                                      "\nHint: type in either 'easy','medium' or 'hard'"); 
                     main();)

let () = main ()