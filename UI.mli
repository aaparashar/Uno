open Deck
(** 
   The main Interface for the game.
*)

(**[style_color color] is the  *)
val style_color: string -> ANSITerminal.style list

(**[pp_card c] is a pretty printed card [c] in the form color number
   in the color of [c] *)
val pp_card : card -> unit

(** [print_hand] prints the player's hand, each on its own line. *)
val print_hand : Deck.t -> unit 

(**[print_card c] is the string form of pp_card [c] *)
val print_card : card -> string

(** [do_play_game] executes the game engine *)
val do_play_game : State.t -> string -> unit

(** [play_game] calls [do_play_game]*)
val play_game : string -> string-> unit

(** [main] prompts the game to begin, and then calls [play_game] *)
val main : unit -> unit
