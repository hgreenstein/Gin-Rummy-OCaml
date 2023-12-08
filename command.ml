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

let parse str =
  let strlst = String.split_on_char ' ' str in
    match strlst with
    | "pass"::[] -> Pass
    | "gin"::[] -> Gin
    | "take"::"from"::"discard"::[] -> TakeDiscard
    | "take"::"from"::"deck"::[] -> TakeStock
    | "new"::"game"::t -> NewGame t
    | "quit"::[] -> Quit
    | "Pass"::[] -> Pass
    | "Gin"::[] -> Gin
    | "Take"::"from"::"discard"::[] -> TakeDiscard 
    | "Take"::"from"::"deck"::[] -> TakeStock 
    | "New"::"game"::t -> NewGame t
    | "Take"::"From"::"discard"::[] -> TakeDiscard 
    | "Take"::"From"::"deck"::[] -> TakeStock
    | "New"::"Game"::t -> NewGame t
    | "Take"::"From"::"Discard"::[] -> TakeDiscard 
    | "Take"::"From"::"Deck"::[] -> TakeStock 
    | "new"::"Game"::t -> NewGame t
    | "take"::"From"::"Discard"::[] -> TakeDiscard
    | "take"::"From"::"Deck"::[] -> TakeStock
    | "Take"::"from"::"Discard"::[] -> TakeDiscard
    | "Take"::"from"::"Deck"::[] -> TakeStock
    | "take"::"from"::"Discard"::[] -> TakeDiscard
    | "take"::"from"::"Deck"::[] -> TakeStock
    | "take"::"From"::"discard"::[] -> TakeDiscard
    | "take"::"From"::"deck"::[] -> TakeStock
    | "draw"::"from"::"discard"::[] -> TakeDiscard
    | "draw"::"from"::"deck"::[] -> TakeStock
    | "Draw"::"From"::"discard"::[] -> TakeDiscard
    | "Draw"::"From"::"deck"::[] -> TakeStock
    | "Draw"::"From"::"Discard"::[] -> TakeDiscard
    | "Draw"::"From"::"Deck"::[] -> TakeStock
    | "Draw"::"from"::"discard"::[] -> TakeDiscard
    | "Draw"::"from"::"deck"::[] -> TakeStock
    | "draw"::"From"::"Discard"::[] -> TakeDiscard
    | "draw"::"From"::"Deck"::[] -> TakeStock
    | "Draw"::"from"::"Discard"::[] -> TakeDiscard
    | "Draw"::"from"::"Deck"::[] -> TakeStock
    | "draw"::"from"::"Discard"::[] -> TakeDiscard
    | "draw"::"from"::"Deck"::[] -> TakeStock
    | "draw"::"From"::"discard"::[] -> TakeDiscard
    | "draw"::"From"::"deck"::[] -> TakeStock
    | "Quit"::[] -> Quit
    | "Drop"::h1::h2::h3::[] -> Drop [h1; h2; h3]
    | "drop"::h1::h2::h3::[] -> Drop [h1; h2; h3]
    | s -> Invalid