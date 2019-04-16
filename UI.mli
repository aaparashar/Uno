open Deck
(** 
   The main Interface for the game.
*)

(** [print_hand] prints the player's hand, each on its own line. *)
val print_hand : Deck.card list -> unit 

(** [do_play_game] executes the game engine *)
val do_play_game : State.t -> unit

(** [play_game] calls [do_play_game]*)
val play_game : string -> unit

(** [main] prompts the game to begin, and then calls [play_game] *)
val main : unit -> unit
