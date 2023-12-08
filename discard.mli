open Card

(**The abstract type of a discard pile stack*)
type 'a p 

(**Creates new discard pile with no cards in it.
   The discard pile is implemented as a stack. Takes in Unit as an input and
   returns a new stack with the top = None*)
val empty : unit -> 'a p

(**Pushes a card to the top of the discard pile stack and returns unit.
   Does not return a new stack because it changes it mutably*)
val push : Card.c -> 'a p -> unit 

(**Pops the top card off of the stack and removes it from the mutable
   stack, then returns unit. Returns just unit and does nothing 
   if the stack is empty*)
val pop : 'a p -> unit 

(**Returns the value of the node at the top of the stack. Fails with
   'Empty Discard Pile' if you try to peek from an empty discard pile. However
   an empty discard pile would violate the gamestate 
   and thus should never occur*) 
val peek : 'a p -> Card.c

(*Checks to see if the discard pile is empty*)
val is_empty : 'a p -> bool