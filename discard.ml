(* An ['a node] is a node of a mutable linked list.  It has
   * a field [value] that contains the node's value, and
   * a mutable field [next] that is [Null] if the node has
   * no successor, or [Some n] if the successor is [n]. *)
type 'a node = {value : Card.c; mutable next : 'a node option}

(* AF: An ['a discard] is a stack represented by a mutable linked list.
 * The mutable field [top] is the first node of the list,
 * which is the top of the stack. The empty stack is represented
 * by {top = None}.  The node {top = Some n} represents the
 * stack whose top is [n], and whose remaining elements are
 * the successors of [n]. *)
type 'a p = {mutable top : 'a node option}

let empty () = 
  {top = None}

let is_empty p =
  if(p.top == None) then true else false

let push c d =
  d.top <- Some {value = c; next = d.top}

let pop d =
  match d.top with 
  | None -> () 
  | Some {next} -> d.top <- next 

let peek d =
  match d.top with
  | None -> failwith ("Empty discard pile")
  | Some {value} -> value 