type suit = Hearts | Diamonds | Clubs | Spades

type rank = int

type prime = int

type c = {

  st : suit;
  rk : rank;
  pm : prime;

}

(**Each card in the 52-card deck is bound to a different prime number. This will be used to check for a winning hand. These are the bindings:
   Ace of Clubs = 2; Ace of Spades = 3; Ace of Hearts = 5; Ace of Diamonds = 7
   Two of Clubs = 11; Two of Spades = 13; Two of Hearts = 17; Two of Diamonds = 19
   Three of Clubs = 23; Three of Spades = 29; Three of Hearts = 31; Three of Diamonds = 37
   Four of Clubs = 41; Four of Spades = 43; Four of Hearts = 47; Four of Diamonds = 53
   Five of Clubs = 59; Five of Spades = 61; Five of Hearts = 67; Five of Diamonds = 71
   Six of Clubs = 73; Six of Spades = 79; Six of Hearts = 83; Six of Diamonds = 89
   Seven of Clubs = 97; Seven of Spades = 101; Seven of Hearts = 103; Seven of Diamonds = 107
   Eight of Clubs = 109; Eight of Spades = 113; Eight of Hearts = 127; Eight of Diamonds = 131
   Nine of Clubs = 137; Nine of Spades = 139; Nine of Hearts = 149; Nine of Diamonds = 151
   Ten of Clubs = 157; Ten of Spades = 163; Ten of Hearts = 167; Ten of Diamonds = 173
   Jack of Clubs = 179; Jack of Spades = 181; Jack of Hearts = 191; Jack of Diamonds = 193
   Queen of Clubs = 197; Queen of Spades = 199; Queen of Hearts = 211; Queen of Diamonds = 223
   King of Clubs = 227; King of Spades = 229; King of Hearts = 233; King of Diamonds = 239
*)

let new_card suit rank prime = {st = suit; rk = rank; pm = prime}

let string_of_suit (input : suit) = 
  match input with
  | Hearts -> "Hearts"
  | Diamonds -> "Diamonds"
  | Clubs -> "Clubs"
  | Spades -> "Spades"

let string_of_rank (input : rank) = 
  match input with
  | 1 -> "Ace"
  | 2 -> "Two"
  | 3 -> "Three"
  | 4 -> "Four"
  | 5 -> "Five"
  | 6 -> "Six"
  | 7 -> "Seven"
  | 8 -> "Eight"
  | 9 -> "Nine"
  | 10 -> "Ten"
  | 11 -> "Jack"
  | 12 -> "Queen"
  | 13 -> "King"
  | _ -> "Invalid Rank"


let get_suit (card : c) : suit = card.st

let get_rank (card : c) : rank = card.rk

let get_prime (card : c) : prime = card.pm

let to_string (card : c) : string = String.concat " " [(string_of_rank  (get_rank card)); "of"; (string_of_suit (get_suit card))]

let to_card_helper_s str : c =
  match str with 
  | "Ace" -> {st = Spades; rk = 1; pm = 3}
  | "Two" -> {st = Spades; rk = 2; pm = 13}
  | "Three" -> {st = Spades; rk = 3; pm = 29}
  | "Four" -> {st = Spades; rk = 4; pm = 43}
  | "Five" -> {st = Spades; rk = 5; pm = 61}
  | "Six" -> {st = Spades; rk = 6; pm = 79}
  | "Seven" -> {st = Spades; rk = 7; pm = 101}
  | "Eight" -> {st = Spades; rk = 8; pm = 113}
  | "Nine" -> {st = Spades; rk = 9; pm = 139}
  | "Ten" -> {st = Spades; rk = 10; pm = 163}
  | "Jack" -> {st = Spades; rk = 11; pm = 181}
  | "Queen" -> {st = Spades; rk = 12; pm = 199}
  | "King" -> {st = Spades; rk = 13; pm = 229} 
  | "ace" -> {st = Spades; rk = 1; pm = 3}
  | "two" -> {st = Spades; rk = 2; pm = 13}
  | "three" -> {st = Spades; rk = 3; pm = 29}
  | "four" -> {st = Spades; rk = 4; pm = 43}
  | "five" -> {st = Spades; rk = 5; pm = 61}
  | "six" -> {st = Spades; rk = 6; pm = 79}
  | "seven" -> {st = Spades; rk = 7; pm = 101}
  | "eight" -> {st = Spades; rk = 8; pm = 113}
  | "nine" -> {st = Spades; rk = 9; pm = 139}
  | "ten" -> {st = Spades; rk = 10; pm = 163}
  | "1" -> {st = Spades; rk = 1; pm = 3}
  | "2" -> {st = Spades; rk = 2; pm = 13}
  | "3" -> {st = Spades; rk = 3; pm = 29}
  | "4" -> {st = Spades; rk = 4; pm = 43}
  | "5" -> {st = Spades; rk = 5; pm = 61}
  | "6" -> {st = Spades; rk = 6; pm = 79}
  | "7" -> {st = Spades; rk = 7; pm = 101}
  | "8" -> {st = Spades; rk = 8; pm = 113}
  | "9" -> {st = Spades; rk = 9; pm = 139}
  | "10" -> {st = Spades; rk = 10; pm = 163}
  | "jack" -> {st = Spades; rk = 11; pm = 181}
  | "queen" -> {st = Spades; rk = 12; pm = 199}
  | "king" -> {st = Spades; rk = 13; pm = 229} 
  | s -> {st = Spades; rk = 14; pm = 229}

let to_card_helper_c str : c =
  match str with 
  | "Ace" -> {st = Clubs; rk = 1; pm = 2}
  | "Two" -> {st = Clubs; rk = 2; pm = 11}
  | "Three" -> {st = Clubs; rk = 3; pm = 23}
  | "Four" -> {st = Clubs; rk = 4; pm = 41}
  | "Five" -> {st = Clubs; rk = 5; pm = 59}
  | "Six" -> {st = Clubs; rk = 6; pm = 73}
  | "Seven" -> {st = Clubs; rk = 7; pm = 97}
  | "Eight" -> {st = Clubs; rk = 8; pm = 109}
  | "Nine" -> {st = Clubs; rk = 9; pm = 137}
  | "Ten" -> {st = Clubs; rk = 10; pm = 157}
  | "Jack" -> {st = Clubs; rk = 11; pm = 179}
  | "Queen" -> {st = Clubs; rk = 12; pm = 197}
  | "King" -> {st = Clubs; rk = 13; pm = 227} 
  | "ace" -> {st = Clubs; rk = 1; pm = 2}
  | "two" -> {st = Clubs; rk = 2; pm = 11}
  | "three" -> {st = Clubs; rk = 3; pm = 23}
  | "four" -> {st = Clubs; rk = 4; pm = 41}
  | "five" -> {st = Clubs; rk = 5; pm = 59}
  | "six" -> {st = Clubs; rk = 6; pm = 73}
  | "seven" -> {st = Clubs; rk = 7; pm = 97}
  | "eight" -> {st = Clubs; rk = 8; pm = 109}
  | "nine" -> {st = Clubs; rk = 9; pm = 137}
  | "ten" -> {st = Clubs; rk = 10; pm = 157}
  | "1" -> {st = Clubs; rk = 1; pm = 2}
  | "2" -> {st = Clubs; rk = 2; pm = 11}
  | "3" -> {st = Clubs; rk = 3; pm = 23}
  | "4" -> {st = Clubs; rk = 4; pm = 41}
  | "5" -> {st = Clubs; rk = 5; pm = 59}
  | "6" -> {st = Clubs; rk = 6; pm = 73}
  | "7" -> {st = Clubs; rk = 7; pm = 97}
  | "8" -> {st = Clubs; rk = 8; pm = 109}
  | "9" -> {st = Clubs; rk = 9; pm = 137}
  | "10" -> {st = Clubs; rk = 10; pm = 157}
  | "jack" -> {st = Clubs; rk = 11; pm = 179}
  | "queen" -> {st = Clubs; rk = 12; pm = 197}
  | "king" -> {st = Clubs; rk = 13; pm = 227}
  | s -> {st = Spades; rk = 14; pm = 229}

let to_card_helper_d str : c =
  match str with 
  | "Ace" -> {st = Diamonds; rk = 1; pm = 7}
  | "Two" -> {st = Diamonds; rk = 2; pm = 19}
  | "Three" -> {st = Diamonds; rk = 3; pm = 37}
  | "Four" -> {st = Diamonds; rk = 4; pm = 53}
  | "Five" -> {st = Diamonds; rk = 5; pm = 71}
  | "Six" -> {st = Diamonds; rk = 6; pm = 89}
  | "Seven" -> {st = Diamonds; rk = 7; pm = 107}
  | "Eight" -> {st = Diamonds; rk = 8; pm = 131}
  | "Nine" -> {st = Diamonds; rk = 9; pm = 151}
  | "Ten" -> {st = Diamonds; rk = 10; pm = 173}
  | "Jack" -> {st = Diamonds; rk = 11; pm = 193}
  | "Queen" -> {st = Diamonds; rk = 12; pm = 223}
  | "King" -> {st = Diamonds; rk = 13; pm = 239} 
  | "ace" -> {st = Diamonds; rk = 1; pm = 7}
  | "two" -> {st = Diamonds; rk = 2; pm = 19}
  | "three" -> {st = Diamonds; rk = 3; pm = 37}
  | "four" -> {st = Diamonds; rk = 4; pm = 53}
  | "five" -> {st = Diamonds; rk = 5; pm = 71}
  | "six" -> {st = Diamonds; rk = 6; pm = 89}
  | "seven" -> {st = Diamonds; rk = 7; pm = 107}
  | "eight" -> {st = Diamonds; rk = 8; pm = 131}
  | "nine" -> {st = Diamonds; rk = 9; pm = 151}
  | "ten" -> {st = Diamonds; rk = 10; pm = 173}
  | "1" -> {st = Diamonds; rk = 1; pm = 7}
  | "2" -> {st = Diamonds; rk = 2; pm = 19}
  | "3" -> {st = Diamonds; rk = 3; pm = 37}
  | "4" -> {st = Diamonds; rk = 4; pm = 53}
  | "5" -> {st = Diamonds; rk = 5; pm = 71}
  | "6" -> {st = Diamonds; rk = 6; pm = 89}
  | "7" -> {st = Diamonds; rk = 7; pm = 107}
  | "8" -> {st = Diamonds; rk = 8; pm = 131}
  | "9" -> {st = Diamonds; rk = 9; pm = 151}
  | "10" -> {st = Diamonds; rk = 10; pm = 173}
  | "jack" -> {st = Diamonds; rk = 11; pm = 193}
  | "queen" -> {st = Diamonds; rk = 12; pm = 223}
  | "king" -> {st = Diamonds; rk = 13; pm = 239} 
  | s -> {st = Spades; rk = 14; pm = 229}

let to_card_helper_h str : c =
  match str with 
  | "Ace" -> {st = Hearts; rk = 1; pm = 5}
  | "Two" -> {st = Hearts; rk = 2; pm = 17}
  | "Three" -> {st = Hearts; rk = 3; pm = 31}
  | "Four" -> {st = Hearts; rk = 4; pm = 47}
  | "Five" -> {st = Hearts; rk = 5; pm = 67}
  | "Six" -> {st = Hearts; rk = 6; pm = 83}
  | "Seven" -> {st = Hearts; rk = 7; pm = 103}
  | "Eight" -> {st = Hearts; rk = 8; pm = 127}
  | "Nine" -> {st = Hearts; rk = 9; pm = 149}
  | "Ten" -> {st = Hearts; rk = 10; pm = 167}
  | "Jack" -> {st = Hearts; rk = 11; pm = 191}
  | "Queen" -> {st = Hearts; rk = 12; pm = 211}
  | "King" -> {st = Hearts; rk = 13; pm = 233}
  | "ace" -> {st = Hearts; rk = 1; pm = 5}
  | "two" -> {st = Hearts; rk = 2; pm = 17}
  | "three" -> {st = Hearts; rk = 3; pm = 31}
  | "four" -> {st = Hearts; rk = 4; pm = 47}
  | "five" -> {st = Hearts; rk = 5; pm = 67}
  | "six" -> {st = Hearts; rk = 6; pm = 83}
  | "seven" -> {st = Hearts; rk = 7; pm = 103}
  | "eight" -> {st = Hearts; rk = 8; pm = 127}
  | "nine" -> {st = Hearts; rk = 9; pm = 149}
  | "ten" -> {st = Hearts; rk = 10; pm = 167}
  | "1" -> {st = Hearts; rk = 1; pm = 5}
  | "2" -> {st = Hearts; rk = 2; pm = 17}
  | "3" -> {st = Hearts; rk = 3; pm = 31}
  | "4" -> {st = Hearts; rk = 4; pm = 47}
  | "5" -> {st = Hearts; rk = 5; pm = 67}
  | "6" -> {st = Hearts; rk = 6; pm = 83}
  | "7" -> {st = Hearts; rk = 7; pm = 103}
  | "8" -> {st = Hearts; rk = 8; pm = 127}
  | "9" -> {st = Hearts; rk = 9; pm = 149}
  | "10" -> {st = Hearts; rk = 10; pm = 167}
  | "jack" -> {st = Hearts; rk = 11; pm = 191}
  | "queen" -> {st = Hearts; rk = 12; pm = 211}
  | "king" -> {st = Hearts; rk = 13; pm = 233}
  | s -> {st = Spades; rk = 14; pm = 229}

let to_card str1 str3 : c =
    match str3 with 
    | "Hearts" -> to_card_helper_h str1
    | "Diamonds" -> to_card_helper_d str1
    | "Clubs" -> to_card_helper_c str1
    | "Spades" -> to_card_helper_s str1
    | "hearts" -> to_card_helper_h str1
    | "diamonds" -> to_card_helper_d str1
    | "clubs" -> to_card_helper_c str1
    | "spades" -> to_card_helper_s str1
    | s -> {st = Spades; rk = 14; pm = 229}