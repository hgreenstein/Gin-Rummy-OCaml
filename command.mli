type object_phrase = string list

type comm = 
  | NewGame of object_phrase
  | TakeDiscard
  | TakeStock
  | Pass
  | Gin
  | Invalid 
  | Drop of object_phrase
  | Quit

(*Parses a string*)
val parse : string -> comm