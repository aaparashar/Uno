(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including the player's hand, the AI's hand, cards that have been placed down,
   and the remaining cards in the deck
*)

(** The abstract type of values representing the game state. *)
type t

exception Invalid_Move
(** [init_state d] is the initial state of the game.
    In that state, the whole game deck is shuffled and each player is dealt seven cards *)
val init_state : t

(** [current_card st] is the last card of the current state [st]
    that has been placed *)
val get_current_card : t -> Deck.card

(** [players_hand st] is the cards in the player's hand at state [st]*)
val get_players_hand: t -> Deck.t

(** [ai_hand st] is the cards in the AI's hand at state [st]*)
val get_ai_hand: t -> Deck.t

(** [draw_deck st] is the remainder of the deck at state [st] from which
    players will draw cards*)
val get_draw_deck: t -> Deck.t

val put : Deck.card -> t -> t

