(** 
   Representation of static card type data.

   This module represents the data stored in adventure files, including
   the rooms and exits.  It handles loading of that data from JSON as well
   as querying the data.
*)

(** The abstract type of values representing cards. *)
type card

(** The abstract type of values representing a deck. *)
type deck

val load_deck : deck

val shuffle : deck -> deck

val deal : deck -> deck

val add_card: card -> deck -> deck

val remove_card: card -> deck ->deck

val top_card: deck ->card

val is_valid: card -> card -> bool