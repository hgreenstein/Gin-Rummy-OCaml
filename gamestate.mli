open Card
open Deck
open Discard

(** the abstract type representing a player *)
type player = {
  name : string;
  cards : Card.c Deck.d;
}

(** the abstract type representing a gamestate *)
type g = {
  mutable dk: Card.c Deck.d;
  mutable discard: Card.c Discard.p;
  mutable players: player list;
}


val start_discard : g -> unit

(**A new game with a new deck and a specified number of players between 2 and 5*)
val new_game: string list -> Card.c list -> g

(**Discards a card from a certain player *)
val discard: g -> string -> Card.c -> unit

val discard2: g -> Card.c -> unit

(*Adds a card*)
val add_card: g -> string -> Card.c -> unit

(**Draws a card from the deck and returns the new deck and card drawn *)
val draw_from_deck: g -> Card.c Deck.d * Card.c

(**Draws a card from the discard and gives it to a certain player *)
val draw_from_discard: g -> Card.c

(*Removes a card from the discard pile*)
val remove_from_discard: g -> unit

(*Updates a deck*)
val update_deck: g -> Card.c Deck.d -> unit

(*Returns a list of players*)
val get_players: g -> player list

(*Starts a new game*)
val new_empty_game: unit -> g

(*Returns the number of players*)
val get_num_players: g -> int

(*Returns a players name*)
val get_name: player -> string

(*Returns a players hand*)
val get_hand: player -> Card.c Deck.d

(*Returns a deck*)
val get_deck: g -> Card.c Deck.d

(*Returns the discard pile*)
val get_discard: g -> Card.c Discard.p