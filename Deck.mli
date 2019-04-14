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

val load_deck : t

val shuffle : t -> t

val load_deck : t

val riffle : t -> t -> t -> t

val multi_riffle : t -> int -> t

val shuffle : t -> t

val deal : t -> t*t

val add_card: card -> t -> t -> t

val remove_card: card -> t ->t

val top_card: t ->card

val is_valid: card -> card -> bool