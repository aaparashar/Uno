Open Deck
Open state
Open ANSITerminal
Open Format

let pp_card (c:card) = 
  ANSITerminal.(string_of_int[c.color] c.number)

let rec print_hand d =
    match d with 
    | h::t -> "\t"^pp_card^"\n" ^ print_hand t
    | [] -> "\n"

let do_play_game st =
    if State.has_won st then
        print_string ("\n You ")
        if st.players_hand = 0 then print_string("Win :D Thanks for playing!")
        else print_string("Lose uwu Better luck next time")
    else 
        print_string("\n Last Play:\t" ^ pp_card(st.current_card))
        print_string("\n In Your Hand:\n"^print_hand(st.players_hand))
        print_string ("\nWhat's your next move?\n")
        print_string  "> "
    let(out_string,cmd) = match Command.parse (read_line()) with
    | exception (Command.Empty) -> ("\nWhat's your next move?\n","");
    | exception (Command.Malformed) -> ("\n\nCommand not recognized\n","");
    | Quit -> ("\n\nSee ya next time!\n")
        exit 0;
    | Score -> ("Score: "^(string_of_int(State.get_current_score st))^"\n", "")
    | Hand -> ("\n In Your Hand:\n"^print_hand(st.players_hand))
    | Put -> (,"put")
  in
