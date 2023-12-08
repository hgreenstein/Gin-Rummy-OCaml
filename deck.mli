open Card

(**The abstract type of values representing sets of cards, or "decks." *)
type ('a) d

(**A deck with 52 cards, one for each combination of rank and suit, in no particular order.
   That is, the cards being ordered or random isn't required. *)
val new_deck : Card.c d

(**Creates a deck out of a list of cards *)
val create_deck : Card.c list -> Card.c d

(**Removes a card from a deck. *)
val remove : Card.c d -> Card.c -> Card.c d

(**Randomizes the order of cards in a given deck.*)
val shuffle_deck : Card.c d -> Card.c d

(**Cuts the deck at a certain value, placing that value as the new top card and
   putting every value below it on top of the previous top card*)
val cut : int -> Card.c d -> Card.c d

(**Removes the top card of a deck and returns that card. 
   Requires: the deck is non-empty.*)
val draw_card : Card.c d -> Card.c d * Card.c

(**Creates an empty deck *)
val new_empty_deck : Card.c d

(*Checks to see if a player has won*)
val check_win : Card.c list -> bool
(** Adds a new card to a deck *)
val add_card : Card.c d -> Card.c -> Card.c d

val deck_to_list : Card.c d -> Card.c list

(*Displays a players hands as asterisks*)
val display_hand: Card.c d -> string

(*Converts the deck to a string*)
val deck_to_string: Card.c d -> string

(*Checks to see if a card is in a deck*)
val is_in: Card.c d -> Card.c -> bool