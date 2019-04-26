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

(** [create_state c pl_h a_h d_d pl_d turn] is a state defined by the values
    [c], [pl_h], [a_h], [d_d], [pl_d] [pl_pl] [ai_pl] [action] [turn]. For testing 
    purposes. *)
val create_state : Deck.card -> Deck.t -> Deck.t -> Deck.t -> Deck.t -> Deck.t -> Deck.t -> string -> bool -> t

(** [init_state d] is the initial state of the game when given a deck [d].
    In that state, the deck is shuffled and each player is dealt seven cards *)
val init_state : t

(** [current_card st] is the last card of the current state [st]
    that has been placed *)
val get_current_card : t -> Deck.card

(** [players_hand st] is the cards in the player's hand at state [st]*)
val get_players_hand : t -> Deck.t

(** [ai_hand st] is the cards in the AI's hand at state [st]*)
val get_ai_hand : t -> Deck.t

(** [draw_deck st] is the remainder of the deck at state [st] from which
    players will draw cards*)
val get_draw_deck : t -> Deck.t

(** [get_playing_deck st] is a deck at state [st] that contains all the cards 
    that have been played so far. *)
val get_playing_deck : t -> Deck.t

(** [get_player_played st] is a deck at state [st] that contains all the cards
    played by the player so far. *)
val get_player_played : t -> Deck.t

(** [get_ai_played st] is a deck at state [st] that contains all the cards
    played by the ai so far. *)
val get_ai_played : t -> Deck.t

(** [get_lastp_action st] is the most recent action of the player. *)
val get_lastp_action : t -> string

(** [get_turn st] is a boolean that indicates whose turn it is. if it is true 
    then it is the players turn. If it is false, the it is the ai's turn*)
val get_turn : t ->bool

(** [put  c st str] is a state after a card c has been put down on to the 
    playing deck by str.
    Raises: [Invalid_Move] if adding c violates one of the rules of Uno *)
val put : Deck.card -> t ->  string -> t

(** [draw st str] is a state after str picks a card from the drawing deck. 
    If the drawing deck was empty then the cards in the playing deck excluding 
    the current card get shuffled and put in the drawing deck. *)
val draw : t -> string -> t

(** [has_won st] is true if either the player or the ai has won the game.
    Otherwise, it is false. *)
val has_won : t -> bool

(**[dumb_ai_turn st] is the state after the dumb ai makes a move. This ai has no
   strategy and picks the first valid card if it has a valid card. Otherwise it 
   draws a card*)
val dumb_ai_turn : t -> t

(**[medium_ai_turn st] is the state after the medium ai makes a move. The medium
   ai uses a basic strategy. It picks the first valid number card. If there is
   no valid number card in it's hand, it picks the the first valid power card.
   If the ai puts down a Wild or Draw Four card, it will choose the color to 
   change to based on what color of card it has the most of in it's hand. *)
val medium_ai_turn : t -> t

(** [supreme_ai turn] is the state after the supreme ai makes a move. The 
    supreme ai employs various elite high iq undefeatable tactics against it's
    unwitting fools. 
    [@see < https://www.unorules.com/best-strategies-to-win-uno/ > 
    {Explanation of strategies here.}] *)
val supreme_ai_turn : t -> t 