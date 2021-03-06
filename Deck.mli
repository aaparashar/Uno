(** 
   Representation of static card type data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)
type color

(** The abstract type of values representing special card's powers. *)
type power

(** The abstract type of values representing power cards. *)
type power_card

(** The abstract type of values representing number cards. *)
type number_card

(** The abstract type of values representing cards. *)
type card

(** The abstract type of values representing a deck. *)
type t

(** Raised when an invalid color is parsed. *)
exception Invalid_Color of string

(** Raised when an invalid color is parsed. *)
exception Invalid_Power of string

(** Raied when there is an attempt to make a malformed card. *)
exception Malformed_Card

(** [color_of_string s] is the color indicated by string [s].
    Raises [Invalid_Color s] if the string does not represent a valid color. *)
val color_of_string : string -> color

(** [power_of_string s] is the power indicated by string [s].
    Raises [Invalid_Power s] if the string does not represent a valid power. *)
val power_of_string : string -> power

(** [type_to_string c] indicates whether card c is a Number Card or a 
    Power Card.*)
val type_to_string : card -> string

(** [create_num_card s n] is the number card with number [n] and the color 
    represented by string [s]. *)
val create_num_card : string -> int -> card

(** [create_pow_card s p] is the power card with power [p] and the color 
    represented by string [s]. *)
val create_pow_card: string -> string -> card

(** [load_num_color numbers col] loads cards with the numbers in  [numbers] and
    color [col]*)
val load_num_color : int list -> string -> card list

(** [load_pow_color powers col] loads cards with the powers in  [powers] and
    color [col]*)
val load_pow_color : string list -> string -> card list

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

(**[card_col c] is the color of card [c] *)
val card_col: card -> string

(** [card_val c] is the number (i.e. int) of num_card [c]. *)
val card_val: card -> int * power 

(** [val_to_string c] is the string form of the value of card [c]. The value
    of [c] is either it's power or number.*)
val val_to_string : card -> string

(** [card_equals c1 c2] is true if and only if c1 and c2 are equivalent cards.*)
val card_equals: card -> card -> bool

(** [add_card c d] is deck [d] with card [c] add to the top *)
val add_card: card -> t -> t 

(** [remove_card c d] is deck [d] with card [c] removed. *)
val remove_card: card -> t ->t

(**[top_card d] is the first element of the card list/deck [d]. *)
val top_card: t ->card

(**[is_valid card1 card2] is true if [card1] matches the color or 
   number of [card2]. *)
val is_valid: card -> card -> bool

(** [len d] is the number of cards in deck [d]. *)
val len: t -> int

(** [is_powercard c] is true if card [c] is a power card. *)
val is_powercard: card -> bool

(** [string_of_color c] is the string form of color c. *)
val string_of_color : color -> string

(** [string_of_power p] is the string form of power p. *)
val string_of_power : power -> string

(** [list_card c] is a tuple that stores the color of card [c] and 
    its number or power. *)
val list_card: card -> string*string

(** [to_list t] is a list of tuples that contains the cards of deck [t]. *)
val to_list: t -> (string*string) list

(** [deck_contains c d] is true if card [c] is in deck [d]. *)
val deck_contains: card -> t -> bool

(** [get_valid_card c d] is the first card in deck [d] that is a valid
    match to card [c] or is None if there is no valid match. *)
val get_valid_card: card ->t -> card option

(** [merge_decks c d] puts all the cards in  deck [d] and deck [c] in a single 
    deck. It maintains duplicates. *)
val merge_decks: t ->t -> t

(** [get_power p] returns the power of card p. *)
val get_power: card -> power

(** [change_wild_color c col] is power card [c] with it's color changed to color 
    [col]. *)
val change_wild_color : card -> string -> card

(** [random_color] is a random color. *)
val random_color : color

(**[majority_color d] counts the number of cards per each color in deck [d] *) 
val majority_color : t -> color list

(**[get_medium_card c d] is the best card given a hand [d] based on what
   matches card [c], prioritizing number cards, and then power cards, saving
   wild cards for last *) 
val get_medium_card : card -> t -> card option

(**[get_supreme_card c d p pp a] is some card to play on top of last card [c]
   given the players hand [p], what the player has played [pp] to see if the player
   has any streaks (consecutive cards of the same color) as well as the ai's hand
   [c]
   Example: if the player has played a lot of one color the ai will try to change 
   the color rather than playing number cards of the same color
   Example: if the ai has a wild card it will change the color to the card it has 
   most of but the player has played least of *) 
val get_supreme_card : card -> t -> t -> t -> string -> card option * color
