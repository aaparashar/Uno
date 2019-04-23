open Deck
type t = {
  current_card : Deck.card;
  players_hand : Deck.t;
  ai_hand : Deck.t;
  draw_deck : Deck.t;
  playing_deck : Deck.t;
  player_played : Deck.t;
  ai_played : Deck.t;
  turn : bool;
} 

exception Invalid_Move

let create_state curr_c pl_h ai_h draw_d pl_d pl_pl ai_pl is_turn =
  {current_card = curr_c; 
   players_hand = pl_h; 
   ai_hand = ai_h;
   draw_deck = draw_d;
   playing_deck = pl_d;
   player_played = pl_pl;
   ai_played = ai_pl;
   turn = is_turn}

(** [find_top c acc d ] checks if the top card c is a number card if it is not 
    it recurses through the rest of deck [d] until it finds the firts number card*)
let rec find_top c acc d= 
  if is_powercard c then find_top (top_card d) (add_card c acc) (remove_card c d)
  else (c , merge_decks d acc)
(* |Num_Card n -> (n, merge_decks d acc)
   |Power_Card p ->  find_top (top_card d) (add_card p acc) (remove_card p d) *)

let init_state  = 
  let deck = shuffle(load_deck) in 
  let players = fst (deal deck) in 
  let ai_deck = fst (deal (snd (deal deck))) in 
  let remaining = snd (deal (snd (deal deck))) in 
  let playing = add_card (top_card remaining) Deck.empty_deck in
  let remaining2 = remove_card (top_card remaining) remaining in
  let top = fst(find_top (top_card playing) empty_deck playing) in 
  let playing = snd (find_top (top_card playing) empty_deck playing) in 
  {current_card = top; players_hand = players; ai_hand= ai_deck; 
   draw_deck=remaining2; playing_deck= playing; player_played = empty_deck; 
   ai_played = empty_deck; turn = true}

let get_current_card (st:t) = st.current_card
let get_players_hand st = st.players_hand
let get_ai_hand st = st.ai_hand
let get_draw_deck st = st.draw_deck
let get_playing_deck st = st.playing_deck
let get_player_played st = st.player_played
let get_ai_played st = st.ai_played
let has_won st = Deck.len st.players_hand = 0 || Deck.len st.ai_hand = 0
(* let get_current_score = None *)
let get_turn st = st.turn

let draw (st:t) s = 
  if (st.draw_deck = empty_deck && s= "player") then 
    let reset = {st with draw_deck=shuffle (remove_card st.current_card st.playing_deck); 
                         playing_deck = add_card st.current_card empty_deck} in 
    {reset with players_hand = (add_card (top_card reset.draw_deck) reset.players_hand); 
                draw_deck = remove_card (top_card reset.draw_deck) reset.draw_deck; turn = false;}
  else if (st.draw_deck = empty_deck && s= "ai") then 
    let reset = {st with draw_deck=shuffle (remove_card st.current_card st.playing_deck); 
                         playing_deck = add_card st.current_card empty_deck} in 
    {reset with ai_hand = (add_card (top_card reset.draw_deck) reset.ai_hand); 
                draw_deck = remove_card (top_card reset.draw_deck) reset.draw_deck;turn=true;}
  else if (s = "ai") then 
    {st with ai_hand = (add_card (top_card st.draw_deck) st.ai_hand); 
             draw_deck = remove_card (top_card st.draw_deck) st.draw_deck;turn=true;}
  else 
    {st with players_hand = (add_card (top_card st.draw_deck) st.players_hand); 
             draw_deck = remove_card (top_card st.draw_deck) st.draw_deck;turn=false;}

(* When wild card is played: 
      Change color of wild card to the intended color and add to playing_deck

   Next card played after wild card:
      If card c is_valid and top card of playing_deck has power = "wild" then
      change color of top card back to wild and then add c to playing_deck *)
let put c (st:t) s = 
  if ((is_valid st.current_card c )&& (s="player") 
      && (deck_contains c st.players_hand)) then

    if not (is_powercard c) then  
      {st with current_card = c;
       players_hand = remove_card c st.players_hand;  
       playing_deck= add_card c st.playing_deck; 
       player_played = add_card c st.player_played;
       turn = false} 
    else
      match (val_to_string c) with
      |"draw two" -> ANSITerminal.(print_string [cyan] 
                                   ("\nThe AI has been dealt 2 cards"));
      let tempSt = draw (draw st "ai") "ai" in
      {tempSt with player_played = add_card c st.player_played;}
      |"draw four" -> begin 
        let st2 = draw (draw(draw (draw st "ai") "ai") "ai") "ai" in

        (*********************AMBITIOUS REFACTORING EFFORT***********************)
        ANSITerminal.(print_string [cyan] ("\nThe AI has been dealt 4 cards.
      \nWhat color do you choose?\n"));
        ANSITerminal.(print_string [white] "> ");
        let wildcol = read_line() in
        try 
          let temp = change_wild_color c wildcol in
          {st2 with current_card = temp;
                    players_hand= remove_card c st.players_hand;  
                    playing_deck= add_card c st.playing_deck;
                    player_played = add_card temp st.player_played;
                    turn = false}
        with 
        | Deck.Invalid_Color malCol ->
          try 
            match wildcol with
            |"skip" -> {st with current_card = c;
                                players_hand = remove_card c st.players_hand;  
                                playing_deck= add_card c st.playing_deck;
                                player_played = add_card c st.player_played; 
                                turn = true} 
            |"reverse" -> {st with current_card = c;
                                   players_hand = remove_card c st.players_hand;  
                                   playing_deck= add_card c st.playing_deck; 
                                   player_played = add_card c st.player_played; 
                                   turn = true} 
            |"wild" -> begin 
              ANSITerminal.(print_string [cyan] ("\nWhat color do you choose?\n"));
              ANSITerminal.(print_string [white] "> ");
              let wildcol = read_line() in
              try
                let temp = change_wild_color c wildcol in
                {st with current_card = temp;
                         players_hand= remove_card c st.players_hand;  
                         playing_deck= add_card c st.playing_deck; 
                         player_played = add_card temp st.player_played; 
                         turn = false}
              with
                | Deck.Invalid_Color malcol -> raise (Invalid_Color malCol)
              end
              | _ -> raise (Invalid_Color wildcol)
          with 
          | _ -> raise Invalid_Move
      end
      | _ -> raise Invalid_Move

          (************************************************************************)  

          (* match read_line() with 
             |"red" -> let temp = change_wild_color c "red" in 
             {st2 with current_card = temp;
                      players_hand= remove_card c st.players_hand;  
                      playing_deck= add_card c st.playing_deck; 
                      turn = false}
             |"blue" -> let temp = change_wild_color c Blue in 
             {st2 with current_card = temp;
                      players_hand = remove_card c st.players_hand;  
                      playing_deck= add_card c st.playing_deck; 
                      turn = false}
             |"green" -> let temp = change_wild_color c Green in 
             {st2 with current_card = temp;
                      players_hand = remove_card c st.players_hand;  
                      playing_deck= add_card c st.playing_deck; 
                      turn = false}
             |"yellow" -> let temp = change_wild_color c Yellow in 
             {st2 with current_card = temp;
                      players_hand = remove_card c st.players_hand;  
                      playing_deck= add_card c st.playing_deck; 
                      turn = false}
             |_ -> raise Invalid_Move
             |Skip -> {st with current_card = c;
                            players_hand = remove_card c st.players_hand;  
                            playing_deck= add_card c st.playing_deck; 
                            turn = true} 
             |Reverse -> {st with current_card = c;
                               players_hand = remove_card c st.players_hand;  
                               playing_deck= add_card c st.playing_deck; 
                               turn = true} 
             |Wild -> ANSITerminal.(print_string [cyan] ("\nWhat color do you choose?\n"));
             ANSITerminal.(print_string [white] "> ");
             match read_line() with 
             |"red" -> let temp = change_wild_color c Red in 
              {st with current_card = temp;
                       players_hand= remove_card c st.players_hand;  
                       playing_deck= add_card c st.playing_deck; 
                       turn = false}
             |"blue" -> let temp = change_wild_color c Blue in 
              {st with current_card = temp;
                       players_hand = remove_card c st.players_hand;  
                       playing_deck= aduyfytu8gfd_card c st.pklaying_deck; 
                       turn = false}
             |"green" -> let temp = change_wild_color c Green in 
              {st with current_card = temp;
                       players_hand = remove_card c st.players_hand;  
                       playing_deck= add_card c st.playing_deck; 
                       turn = false}
             |"yellow" -> let temp = change_wild_color c Yellow in 
              {st with current_card = ;;temp;
             players_hand = remove_card c st.players_hand;  
             playing_deck= add_card c st.playing_deck; 
             turn = false}
             |_ -> raise Invalid_Move *)
          (* |"skip" -> {st with current_card = c;
                              players_hand = remove_card c st.players_hand;  
                              playing_deck= add_card c st.playing_deck; 
                              turn = true} 
          |"reverse" -> {st with current_card = c;
                                 players_hand = remove_card c st.players_hand;  
                                 playing_deck= add_card c st.playing_deck; 
                                 turn = false}
          |_ -> raise Invalid_Move *)
  else if (is_valid c st.current_card && s="ai"&& deck_contains c st.ai_hand)
  then 
    if not (is_powercard c) then
      {st with current_card = c;
       ai_hand = remove_card c st.ai_hand;  
       playing_deck= add_card c st.playing_deck; 
       ai_played = add_card c st.ai_played; 
       turn = false} 
    else match (val_to_string c) with
      |"draw two" -> let st = draw (draw st "player") "player" in
                     {st with ai_played = add_card c st.ai_played}
      |"draw four" -> begin
        let st2 =
          draw (draw (draw (draw st "player") "player") "player") "player" in 
        ANSITerminal.(print_string [cyan] ("\nThe AI hit you with a draw 4\n"));
        let col = string_of_color random_color in
        let temp = change_wild_color c col in 
        ANSITerminal.(print_string [cyan]("\n AI changes the color to " ^ col));
        {st2 with current_card = temp;
         ai_hand = remove_card c st.ai_hand;  
         playing_deck= add_card c st.playing_deck; 
         ai_played = add_card temp st.ai_played; 
         turn = true}
      end
      |"skip" -> {st with current_card = c;
                  ai_hand = remove_card c st.ai_hand;  
                  playing_deck= add_card c st.playing_deck; 
                  ai_played = add_card c st.ai_played; 
                  turn = false} 
      |"reverse" ->{st with current_card = c;
                    ai_hand = remove_card c st.ai_hand;  
                    playing_deck= add_card c st.playing_deck;
                    ai_played = add_card c st.ai_played; 
                    turn = false} 

      |"wild"-> let col = string_of_color random_color in 
        let temp = change_wild_color c col in 
        ANSITerminal.(print_string [cyan]("\n AI changes the color to " ^ col));
        {st with current_card = temp;
         ai_hand = remove_card c st.ai_hand;  
         playing_deck= add_card c st.playing_deck;
         ai_played = add_card temp st.ai_played;  
         turn = true}
         
  else raise Invalid_Move

(**TODO change random_color to color that AI has most of *)
(* let put_smart_ai c st =
   if (is_valid c st.current_card && deck_contains c st.ai_hand)
   then match c with 
    |Num_Card ->{st with current_card = c;
                         ai_hand = remove_card c st.ai_hand;  
                         playing_deck= add_card c st.playing_deck; 
                         turn = false} 
    |Power_Card p -> match p.power with
      |Draw_Two -> draw (draw st "player") "player"
      |Draw_Four -> let st2 = 
                      draw (draw(draw (draw st "player") "player") "player") "player" in 
        ANSITerminal.(print_string [cyan] ("\nThe AI hit you with a draw 4\n"));

        let col = majority_color ai_hand in 
        let temp = change_wild_color c col in 
        {st2 with current_card = temp;
                  ai_hand = remove_card c st.ai_hand;  
                  playing_deck= add_card c st.playing_deck; 
                  turn = true}
          ANSITerminal.(print_string [cyan]("\n AI changes the color to "
                                            ^ (color_to_string col)));

      |Skip -> {st with current_card = c;
                        ai_hand = remove_card c st.ai_hand;  
                        playing_deck= add_card c st.playing_deck; 
                        turn = false} 
      |Reverse ->{st with current_card = c;
                          ai_hand = remove_card c st.ai_hand;  
                          playing_deck= add_card c st.playing_deck; 
                          turn = false} 

      |Wild -> let col = random_color in 
        let temp = change_wild_color c col in 
        {st with current_card = temp;
                 ai_hand = remove_card c st.ai_hand;  
                 playing_deck= add_card c st.playing_deck; 
                 turn = true}
          ANSITerminal.(print_string [cyan]("\n AI changes the color to "
                                            ^ (color_to_string col)));


   else raise Invalid_Move
*)


let dumb_ai_turn st = 
  match (get_valid_card st.current_card st.ai_hand) with
  |None -> draw st "ai"
  |Some x -> put x st "ai"

(**TODO change get valid card to get a card strategically  make another
   method in deck and then replace it here*)
let smart_ai_turn st = 
  match (get_valid_card st.current_card st.ai_hand) with
  |None -> draw st "ai"
  |Some x -> put_smart_ai x st 




