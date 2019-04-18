(** 
   Representation of dynamic game state.

   This module represents the state of a game as it is being played,
   including the player's hand, the AI's hand, cards that have been placed down,
   and the remaining cards in the deck
*)


open Deck
(** The abstract type of values representing the game state. *)
type t

(** Raised when a move that would violate the rules of the game is made
    i.e putting a card not in your hand. *)
exception Invalid_Move

(** [init_state d] is the initial state of the game when given a deck [d].
    In that state, the deck is shuffled and each player is dealt seven cards *)
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

(** [get_playing_deck st] is a deck at state [st] that contains all the cards 
    that have been played so far. *)
val get_playing_deck: t -> Deck.t

(** [put  c st str] is a state after a card c has been put down on to the 
    playing deck by str.
    Raises: [Invalid_Move] if adding c violates one of the rules of Uno *)
val put: Deck.card -> t ->  string -> t

(** [draw st str] is a state after str picks a card from the drawing deck. 
    If the drawing deck was empty then the cards in the playing deck excluding 
    the current card get shuffled and put in the drawing deck. *)
val draw: t -> string -> t

(**[ai_turn st] is a state after the ai makes a move. Currently the ai has no
   strategy and picks the first valid card if it has a valid card  otherwise it 
   draws a card*)
val ai_turn: t -> t

(** [get_turn st] is a boolean that indicates whose turn it is. if it is true 
    then it is the players turn. If it is false, the it is the ai's turn*)
val get_turn: t ->bool

val has_won: t -> bool