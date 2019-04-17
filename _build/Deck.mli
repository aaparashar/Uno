(** 
   Representation of static card type data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(** The abstract type of values representing cards. *)
type card

(** The abstract type of values representing a deck. *)
type t

(** [create card color number] is a card with color [color] and number 
    [number]. *)
val create_card : string -> int -> card 

(** [empty_deck] is an empty deck. *)
val empty_deck : t

(** [load_deck] is a deck consisting of four colors –– Red, Blue, Yellow,
    Green –– and each color consists of one zero card, and 
    two of each 1 through 9 *)
val load_deck : t

(** [riffle d acc_a acc_b] is a deck riffle shuffled by dividing deck [d]
      into two random piles [acc_a] and [acc_b]*)
val riffle : t -> t -> t -> t

(**[multi_riffle d k] is deck [d] riffle shuffled [k] times *)
val multi_riffle : t -> int -> t

(** [shuffle] is deck [d] shuffled a random number [1,7] times *)
val shuffle : t -> t

(** [deal d] is a hand of seven cards dealt from deck [d] *)
val deal : t -> t*t

(** [add_card c d] is deck [d] with card [c] add to the top *)
val add_card: card -> t -> t 

(** [remove_card c d] is deck [d] with card [c] removed *)
val remove_card: card -> t ->t

(**[top_card d] is the first element of the card list/deck [d] *)
val top_card: t ->card

(**[is_valid card1 card2] is true if [card1] matches the color or 
   number of [card2] *)
val is_valid: card -> card -> bool

(** [len d] is the number of cards in deck [d] *)
val len: t -> int

(**[card_num c] is the number (i.e. int) of card [c] *)
val card_num: card -> int

(**[card_col c] is the color of card [c] *)
val card_col: card -> string

(** [list_card c] is a tuple that stores the color of card [c] and its number *)
val list_card: card -> int*string

(** [to_list t] is a list of tuples that contains the cards of deck [t] *)
val to_list: t -> (int*string) list

(** [contains c d] is true if card [c] is in deck [d] *)
val contains: card -> t -> bool

(** [get_valid_card c d] is the first card in deck [d] that is a valid
   match to card [c] or is None if there is no valid match *)
val get_valid_card: card ->t -> card option
