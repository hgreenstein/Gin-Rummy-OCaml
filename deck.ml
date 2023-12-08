open Card 

type ('a) d = ('a) list

let deck_to_list (deck : Card.c d) : Card.c list =
  deck

let rec is_in deck card = 
  match (deck_to_list deck) with 
  | h::t -> if ((Card.get_prime h)==(Card.get_prime card)) 
    then true 
    else is_in t card
  | [] -> false

let rec deck_to_string_helper dk =
  match dk with 
  | h::t -> (Card.to_string h)::deck_to_string_helper t
  | [] -> []

let deck_to_string (dk : Card.c d) =
  match dk with
  | h::t -> String.concat "; " ((Card.to_string h)::(deck_to_string_helper t))
  | [] -> ""

let new_deck : c d = 
  [new_card Clubs 1 2; new_card Spades 1 3; 
   new_card Hearts 1 5; new_card Diamonds 1 7;
   new_card Clubs 2 11; new_card Spades 2 13;
   new_card Hearts 2 17; new_card Diamonds 2 19;
   new_card Clubs 3 23; new_card Spades 3 29; 
   new_card Hearts 3 31; new_card Diamonds 3 37;
   new_card Clubs 4 41; new_card Spades 4 43; 
   new_card Hearts 4 47; new_card Diamonds 4 53;
   new_card Clubs 5 59; new_card Spades 5 61; 
   new_card Hearts 5 67; new_card Diamonds 5 71;
   new_card Clubs 6 73; new_card Spades 6 79; 
   new_card Hearts 6 83; new_card Diamonds 6 89;
   new_card Clubs 7 97; new_card Spades 7 101; 
   new_card Hearts 7 103; new_card Diamonds 7 107;
   new_card Clubs 8 109; new_card Spades 8 113; 
   new_card Hearts 8 127; new_card Diamonds 8 131;
   new_card Clubs 9 137; new_card Spades 9 139; 
   new_card Hearts 9 149; new_card Diamonds 9 151;
   new_card Clubs 10 157; new_card Spades 10 163; 
   new_card Hearts 10 167; new_card Diamonds 10 173;
   new_card Clubs 11 179; new_card Spades 11 181; 
   new_card Hearts 11 191; new_card Diamonds 11 193;
   new_card Clubs 12 197; new_card Spades 12 199; 
   new_card Hearts 12 211; new_card Diamonds 12 223;
   new_card Clubs 13 227; new_card Spades 13 229; 
   new_card Hearts 13 233; new_card Diamonds 13 229;
  ]

let new_empty_deck : c d = 
  []

let create_deck (lst : c list) : c d = 
  lst

(** Removes element x from List lst
    lst must be a List that contains x
    x is an element of lst *)
let rec remove (lst : Card.c d) x =
  match lst with
  | [] -> lst
  | h::t -> if h = x then t else h::(remove t x)

(** Draws the first card from List lst
    lst must be a List of cards
    returns new list and drawn card *)
let rec draw_card lst : c d * c =
  match lst with
  | [] -> lst, new_card Hearts 1 5
  | h::t -> remove lst h, h

(**Makes a list of all elements in lst before n, not including n
   n is and int
   count is an int and must = 0
   lst is a list*)
let rec frontsplit n count lst =
  match lst with
  | [] -> lst
  | h::t -> if n > count then h::(frontsplit n (count+1) t) 
    else frontsplit n (count+1) t

(**Makes a list of all elements in lst of n and all elements after
   n is and int
   count is an int and must = 0
   lst is a list*)
let rec backsplit n count lst =
  match lst with
  | [] -> lst
  | h::t -> if count >= n then h::t else backsplit n (count+1) t

(**Cuts the deck at n, making n the new top of the list
   n is an int
   lst is a list
*)
let cut n lst =
  List.append (backsplit n 0 lst) (frontsplit n 0 lst)

(**Randomly shuffles the elements of List lst into a new List
   lst is a List *)


let rec shuffle_deck lst = 
  let x = Random.self_init () in
  match lst with
  |[] -> lst
  |h::t -> let n =  Random.int (List.length lst) in
    (List.nth lst n)::(shuffle_deck (remove lst (List.nth lst n)))

(**A list of unique integers representing the valid melds of gin rummy. 
   I could simplify these expressions, but that would take a long time, and
   Ocaml doesn't actually care, so I'm not gonna.*)
let melds = [2 * 3 * 5 * 7;
             11 * 13 * 17 * 19;
             23 * 29 * 31 * 37;
             41 * 43 * 47 * 53;
             59 * 61 * 67 * 71;
             73 * 79 * 83 * 89;
             97 * 101 * 103 * 107;
             109 * 113 * 127 * 131;
             137 * 139 * 149 * 151;
             157 * 163 * 167 * 173;
             179 * 181 * 191 * 193;
             197 * 199 * 211 * 223;
             227 * 229 * 233 * 239;
             2 * 11 * 23 * 41;
             11 * 23 * 41 * 59;
             23 * 41 * 59 * 73;
             41 * 59 * 73 * 97;
             59 * 73 * 97 * 109;
             73 * 97 * 109 * 137;
             97 * 109 * 137 * 157;
             109 * 137 * 157 * 179;
             137 * 157 * 179 * 197;
             157 * 179 * 197 * 227;
             3 * 13 * 29 * 43;
             13 * 29 * 43 * 61;
             29 * 43 * 61 * 79;
             43 * 61 * 79 * 101;
             61 * 79 * 101 * 113;
             79 * 101 * 113 * 139;
             101 * 113 * 139 * 163;
             113 * 139 * 163 * 181;
             139 * 163 * 181 * 199;
             163 * 181 * 199 * 229;
             5 * 17 * 31 * 47;
             17 * 31 * 47 * 67;
             31 * 47 * 67 * 83;
             47 * 67 * 83 * 103;
             67 * 83 * 103 * 127;
             83 * 103 * 127 * 149;
             103 * 127 * 149 * 167;
             127 * 149 * 167 * 191;
             149 * 167 * 191 * 211;
             167 * 191 * 211 * 233;
             7 * 19 * 37 * 53;
             19 * 37 * 53 * 71;
             37 * 53 * 71 * 89;
             53 * 71 * 89 * 107;
             71 * 89 * 107 * 131;
             89 * 107 * 131 * 151;
             107 * 131 * 151 * 173;
             131 * 151 * 173 * 193;
             151 * 173 * 193 * 223;
             173 * 193 * 223 * 239;
             3 * 5 * 7;
             2 * 5 * 7;
             2 * 3 * 7;
             2 * 3 * 5;
             13 * 17 * 19;
             11 * 17 * 19;
             11 * 13 * 19;
             11 * 13 * 17;
             29 * 31 * 37;
             23 * 31 * 37;
             23 * 29 * 37;
             23 * 29 * 31;
             43 * 47 * 53;
             41 * 47 * 53;
             41 * 43 * 53;
             41 * 43 * 47;
             61 * 67 * 71;
             59 * 67 * 71;
             59 * 61 * 71;
             59 * 61 * 67;
             79 * 83 * 89;
             73 * 83 * 89;
             73 * 79 * 89;
             73 * 79 * 83;
             101 * 103 * 107;
             97 * 103 * 107;
             97 * 101 * 107;
             97 * 101 * 103;
             113 * 127 * 131;
             109 * 127 * 131;
             109 * 113 * 131;
             109 * 113 * 127;
             139 * 149 * 151;
             137 * 149 * 151;
             137 * 139 * 151;
             137 * 139 * 149;
             163 * 167 * 173;
             157 * 167 * 173;
             157 * 163 * 173;
             157 * 163 * 167;
             181 * 191 * 193;
             179 * 191 * 193;
             179 * 181 * 193;
             179 * 181 * 191;
             199 * 211 * 223;
             197 * 211 * 223;
             197 * 199 * 223;
             197 * 199 * 211;
             229 * 233 * 239;
             227 * 233 * 239;
             227 * 229 * 239;
             227 * 229 * 233;
             2 * 11 * 23;
             11 * 23 * 41;
             23 * 41 * 59;
             41 * 59 * 73;
             59 * 73 * 97;
             73 * 97 * 109;
             97 * 109 * 137;
             109 * 137 * 157;
             137 * 157 * 179;
             157 * 179 * 197;
             179 * 197 * 227;
             3 * 13 * 29;
             13 * 29 * 43;
             29 * 43 * 61;
             43 * 61 * 79;
             61 * 79 * 101;
             79 * 101 * 113;
             101 * 113 * 139;
             163 * 181 * 199;
             181 * 199 * 229;
             5 * 17 * 31;
             17 * 31 * 47;
             31 * 47 * 67;
             47 * 67 * 83;
             67 * 83 * 103;
             83 * 103 * 127;
             103 * 127 * 149;
             127 * 149 * 167;
             149 * 167 * 191;
             167 * 191 * 211;
             191 * 211 * 233;
             7 * 19 * 37;
             19 * 37 * 53;
             37 * 53 * 71;
             53 * 71 * 89;
             71 * 89 * 107;
             89 * 107 * 131;
             107 * 131 * 151;
             131 * 151 * 173;
             151 * 173 * 193;
             173 * 193 * 223;
             193 * 223 * 239;]



let rec prime_product hand = match hand with
  | [] -> 1
  | h :: t -> (Card.get_prime h) * (prime_product t)

let rec check_melds (product : int) (meldList : int list) = match meldList with
  | [] -> false
  | h :: t -> 
    if product mod h = 0 then
      if product / h = 1 
      then true
      else check_melds (product / h) t
    else check_melds product t

let check_win hand = check_melds (prime_product hand) melds


(** Adds a new card to a deck *)
let add_card dk cd = 
  cd :: dk


let display_row_ten suit =
  match suit with
  | Hearts -> "*                 *"
  | Diamonds -> "*          *      *"
  | Clubs -> "*        * * *    *"
  | Spades -> "*        * * *    *"

let display_row_nine suit =
  match suit with
  | Hearts -> "*          *      *"
  | Diamonds -> "*        * * *    *"
  | Clubs -> "*          *      *"
  | Spades -> "*          *      *"


let display_row_eight suit = 
  match suit with
  | Hearts -> "*        * * *    *"
  | Diamonds -> "*      * * * * *  *"
  | Clubs -> "*      * * * * *  *"
  | Spades -> "*      * * * * *  *"

let display_row_seven suit =
  match suit with
  | Hearts -> "*      * * * * *  *"
  | Diamonds -> "*        * * *    *"
  | Clubs -> "*      * * * * *  *"
  | Spades -> "*        * * *    *"

let display_row_six suit = 
  match suit with
  | Hearts -> "*        *   *    *"
  | Diamonds -> "*          *      *"
  | Clubs -> "*        * * *    *"
  | Spades -> "*          *      *"

let display_row_five i = 
  match i with
  | 1 -> "*  *   *          *"
  | 2 -> "*  * * *          *"
  | 3 -> "*  * * *          *"
  | 4 -> "*      *          *"
  | 5 -> "*  * * *          *"
  | 6 -> "*  * * *          *"
  | 7 -> "*      *          *"
  | 8 -> "*  * * *          *"
  | 9 -> "*      *          *"
  | 10 -> "*  *  ***         *"
  | 11 -> "*  * * *          *"
  | 12 -> "*       *         *"
  | 13 -> "*  *   *          *"
  | _ -> "Invalid Rank"

let display_row_four i =
  match i with
  | 1 -> "*  *   *          *"
  | 2 -> "*  *              *"
  | 3 -> "*      *          *"
  | 4 -> "*      *          *"
  | 5 -> "*      *          *"
  | 6 -> "*  *   *          *"
  | 7 -> "*      *          *"
  | 8 -> "*  *   *          *"
  | 9 -> "*      *          *"
  | 10 -> "*  *  * *         *"
  | 11 -> "*      *          *"
  | 12 -> "*  * * *          *"
  | 13 -> "*  * *            *"
  | _ -> "Invalid Rank"


let display_row_three i = 
  match i with 
  | 1 -> "*  * * *          *"
  | 2 -> "*  * * *          *"
  | 3 -> "*  * * *          *"
  | 4 -> "*  * * *          *"
  | 5 -> "*  * * *          *"
  | 6 -> "*  * * *          *"
  | 7 -> "*      *          *"
  | 8 -> "*  * * *          *"
  | 9 -> "*  * * *          *"
  | 10 -> "*  *  * *         *"
  | 11 -> "*      *          *"
  | 12 -> "*  * * *          *"
  | 13 -> "*  *              *"
  | _ -> "Invalid Rank"

let display_row_two i = 
  match i with
  | 1 -> "*  *   *          *"
  | 2 -> "*      *          *"
  | 3 -> "*      *          *"
  | 4 -> "*  *   *          *"
  | 5 -> "*  *              *"
  | 6 -> "*  *              *"
  | 7 -> "*      *          *"
  | 8 -> "*  *   *          *"
  | 9 -> "*  *   *          *"
  | 10 -> "*  *  * *         *"
  | 11 -> "*      *          *"
  | 12 -> "*  *   *          *"
  | 13 -> "*  * *            *"
  | _ -> "Invalid Rank"



let display_row_one i =
  match i with
  | 1 -> "*  * * *          *"
  | 2 -> "*  * * *          *"
  | 3 -> "*  * * *          *"
  | 4 -> "*  *   *          *"
  | 5 -> "*  * * *          *"
  | 6 -> "*  * * *          *"
  | 7 -> "*  * * *          *"
  | 8 -> "*  * * *          *"
  | 9 -> "*  * * *          *"
  | 10 -> "*  *  ***         *"
  | 11 -> "*      *          *"
  | 12 -> "*  * * *          *"
  | 13 -> "*  *   *          *"
  | _ -> "Invalid Rank"


let display_hand dk = 
  "* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *\n"^
  display_row_one (get_rank (List.nth dk 0)) ^ display_row_one (get_rank (List.nth dk 1)) ^ display_row_one (get_rank (List.nth dk 2)) ^ display_row_one (get_rank (List.nth dk 3)) ^ display_row_one (get_rank (List.nth dk 4))^display_row_one (get_rank (List.nth dk 5)) ^ display_row_one (get_rank (List.nth dk 6)) ^"\n"^
  display_row_two (get_rank (List.nth dk 0)) ^ display_row_two (get_rank (List.nth dk 1))^ display_row_two (get_rank (List.nth dk 2)) ^ display_row_two (get_rank (List.nth dk 3)) ^ display_row_two (get_rank (List.nth dk 4)) ^ display_row_two (get_rank (List.nth dk 5)) ^ display_row_two (get_rank (List.nth dk 6))^"\n" ^
  display_row_three (get_rank (List.nth dk 0)) ^ display_row_three (get_rank (List.nth dk 1)) ^ display_row_three (get_rank (List.nth dk 2)) ^ display_row_three (get_rank (List.nth dk 3))^ display_row_three (get_rank (List.nth dk 4)) ^ display_row_three (get_rank (List.nth dk 5)) ^ display_row_three (get_rank (List.nth dk 6)) ^"\n"^
  display_row_four (get_rank (List.nth dk 0)) ^ display_row_four (get_rank (List.nth dk 1)) ^ display_row_four (get_rank (List.nth dk 2)) ^ display_row_four (get_rank (List.nth dk 3)) ^ display_row_four (get_rank (List.nth dk 4)) ^ display_row_four (get_rank (List.nth dk 5)) ^ display_row_four (get_rank (List.nth dk 6)) ^ "\n" ^
  display_row_five (get_rank (List.nth dk 0)) ^ display_row_five (get_rank (List.nth dk 1)) ^ display_row_five (get_rank (List.nth dk 2)) ^ display_row_five (get_rank (List.nth dk 3)) ^ display_row_five (get_rank (List.nth dk 4)) ^ display_row_five (get_rank (List.nth dk 5)) ^ display_row_five (get_rank (List.nth dk 6))  ^"\n"^
  display_row_six (get_suit (List.nth dk 0)) ^ display_row_six (get_suit (List.nth dk 1)) ^ display_row_six (get_suit (List.nth dk 2)) ^ display_row_six (get_suit (List.nth dk 3)) ^ display_row_six (get_suit (List.nth dk 4)) ^display_row_six (get_suit (List.nth dk 5)) ^ display_row_six (get_suit (List.nth dk 6))  ^"\n"^
  display_row_seven (get_suit (List.nth dk 0)) ^ display_row_seven (get_suit (List.nth dk 1))^ display_row_seven (get_suit (List.nth dk 2)) ^ display_row_seven (get_suit (List.nth dk 3)) ^ display_row_seven (get_suit (List.nth dk 4)) ^ display_row_seven (get_suit (List.nth dk 5)) ^ display_row_seven (get_suit (List.nth dk 6))^ "\n" ^
  display_row_eight (get_suit (List.nth dk 0)) ^ display_row_eight (get_suit (List.nth dk 1)) ^ display_row_eight (get_suit (List.nth dk 2)) ^ display_row_eight (get_suit (List.nth dk 3))^ display_row_eight (get_suit (List.nth dk 4)) ^display_row_eight (get_suit (List.nth dk 5)) ^ display_row_eight (get_suit (List.nth dk 6))  ^"\n"^
  display_row_nine (get_suit (List.nth dk 0)) ^ display_row_nine (get_suit (List.nth dk 1)) ^ display_row_nine (get_suit (List.nth dk 2)) ^ display_row_nine (get_suit (List.nth dk 3)) ^ display_row_nine (get_suit (List.nth dk 4)) ^ display_row_nine (get_suit (List.nth dk 5)) ^ display_row_nine (get_suit (List.nth dk 6)) ^"\n" ^
  display_row_ten (get_suit (List.nth dk 0)) ^ display_row_ten (get_suit (List.nth dk 1)) ^ display_row_ten (get_suit (List.nth dk 2)) ^ display_row_ten (get_suit (List.nth dk 3)) ^ display_row_ten (get_suit (List.nth dk 4)) ^display_row_ten (get_suit (List.nth dk 5)) ^ display_row_ten (get_suit (List.nth dk 6)) ^"\n"^
  "* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *\n"





