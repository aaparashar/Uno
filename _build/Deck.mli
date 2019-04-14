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

<<<<<<< HEAD
val riffle : t -> t -> t -> t

val multi_riffle : t -> int -> t

=======
>>>>>>> f84cc74e3a9cc724a31d4fdab5e4f8a10d1bb07d
val shuffle : t -> t

val deal : t -> t*t

val add_card: card -> t -> t 

val remove_card: card -> t ->t

val top_card: t ->card

val is_valid: card -> card -> bool

val empty_deck: t