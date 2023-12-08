
(** The abstract type of values representing cards. A card has a suit and a rank.*)
type c

(**The type of suits. Only four options here: cards without suits aren't used in Gin Rummy.*)
type suit = Hearts | Diamonds | Clubs | Spades

(**The type of ranks. An int between 1 and 13 inclusive.
   We handle these differently than suit because it's sometimes useful to have the integer value. *)
type rank = int

type prime = int

(*Returns a new card with a specific rank and suit*)
val new_card : suit -> rank -> prime -> c

(**returns the suit of a card c. *)
val get_suit : c -> suit

(**returns the rank of a card c. *)
val get_rank : c -> rank

(*Return the prime of a card*)
val get_prime : c -> prime

(**returns a string representing a card c. *)
val to_string : c -> string

(*returns the card representation of a string*)
val to_card : string -> string -> c