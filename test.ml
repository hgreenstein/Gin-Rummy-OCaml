open OUnit2 
open Card 
open Deck

(** This testing file is used to try individual functions that
    go into making the larger game. Many functions could not be tested in
    the formal setting of a testing file because the shuffles of the decks were
    inherently randomized and using seed randomization was determined to be less
    efficient than using the utop interface to test manually. The testing method
    used was test driven developement. Often the test cases were not made by the 
    person who ended up making the function and thus it was up to the implementer
    to make sure their function passed all the test cases and thus they had 
    done their job correctly.*)

let test_draw_card 
    (name: string)
    (list: Card.c d)
    (expected_output: Card.c d * Card.c) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.draw_card list)) 

let test_get_suit
    (name: string)
    (card: Card.c)
    (expected_output: Card.suit) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Card.get_suit card))

let test_get_rank
    (name: string)
    (card: Card.c)
    (expected_output: Card.rank) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Card.get_rank card))

let test_get_prime
    (name: string)
    (card: Card.c)
    (expected_output: int) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Card.get_prime card))

let test_to_string
    (name: string)
    (card: Card.c)
    (expected_output: string) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Card.to_string card))

let test_check_win
    (name: string)
    (hand: Card.c list)
    (expected_output: bool) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.check_win hand))

let test_is_in
    (name: string)
    (deck: Card.c Deck.d)
    (card: Card.c)
    (expected_output: bool) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.is_in deck card))

let test_remove
    (name: string)
    (deck: Card.c Deck.d)
    (card: Card.c)
    (expected_output: Card.c Deck.d) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.remove deck card))

let test_add_card
    (name: string)
    (deck: Card.c Deck.d)
    (card: Card.c)
    (expected_output: Card.c Deck.d) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.add_card deck card))

  let test_to_card
    (name: string)
    (str1: string)
    (str2: string)
    (expected_output: Card.c) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Card.to_card str1 str2))

let test_deck_to_string
    (name: string)
    (deck: Card.c Deck.d)
    (expected_output: string) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.deck_to_string deck))

let test_display_hand
    (name: string)
    (hand: Card.c Deck.d)
    (expected_output: string) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.display_hand hand) 
        ~printer: (fun x -> x))

let test_draw_from_deck
    (name: string)
    (gmst: Gamestate.g)
    (expected_output: (Card.c Deck.d * Card.c)) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Gamestate.draw_from_deck gmst))

let test_get_num_players
    (name: string)
    (gmst: Gamestate.g)
    (expected_output: int) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Gamestate.get_num_players gmst))

let test_get_name
    (name: string)
    (plyr: Gamestate.player)
    (expected_output: string) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Gamestate.get_name plyr))

let test_get_hand
    (name: string)
    (plyr: Gamestate.player)
    (expected_output: Card.c Deck.d) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Gamestate.get_hand plyr))

let test_get_deck
    (name: string)
    (gmst: Gamestate.g)
    (expected_output: Card.c Deck.d) : OUnitTest.test =
  name >:: (fun _ -> 
      assert_equal expected_output (Gamestate.get_deck gmst))
  

let testDeck = Deck.create_deck [Card.new_card Card.Clubs 1 2; Card.new_card Card.Spades 1 3; Card.new_card Card.Hearts 1 5; Card.new_card Card.Diamonds 1 7;]

let testDeck2 = Deck.create_deck [Card.new_card Card.Spades 1 3; Card.new_card Card.Hearts 1 5; Card.new_card Card.Diamonds 1 7;]

let testDeck3 = Deck.create_deck [Card.new_card Card.Hearts 1 5; Card.new_card Card.Diamonds 1 7;]

let testDeck4 = Deck.create_deck [Card.new_card Card.Diamonds 1 7;]

let card1 = Card.new_card Card.Clubs 1 2
let card2 = Card.new_card Card.Diamonds 1 7
let card3 = Card.new_card Card.Spades 1 3
let card4 = Card.new_card Card.Hearts 1 5
let card5 = Card.new_card Card.Spades 13 229 
let card6 = Card.new_card Card.Spades 6 79  
let card7 = Card.new_card Card.Hearts 6 83
let card8 = Card.new_card Card.Diamonds 9 151

let deck = Deck.new_deck

let gamestate = Gamestate.new_game ["a"; "b"; "c"] (Deck.deck_to_list deck)

let gamestate3 = Gamestate.new_game ["a"; "b"; "c"; "d"; "e"] (Deck.deck_to_list deck)

let deck2 = Deck.remove deck card6

let deck4 = Deck.remove (Gamestate.get_deck gamestate) card6

let gamestate2 = Gamestate.new_game ["a"; "b"; "c"] (Deck.deck_to_list deck2)

let deck3 = Deck.remove deck card7

let deck6 = Deck.remove (Gamestate.get_deck gamestate) card7

let gamestate4 = Gamestate.new_game ["a"; "b"] (Deck.deck_to_list deck3)

let deck5 = Deck.remove (Gamestate.get_deck gamestate2) card7

let testhand1 = [Card.new_card Card.Spades 1 3; 
                 Card.new_card Card.Spades 2 13;
                 Card.new_card Card.Spades 3 29; 
                 Card.new_card Card.Spades 4 43; 
                 Card.new_card Card.Clubs 13 227; 
                 Card.new_card Card.Spades 13 229; 
                 Card.new_card Card.Diamonds 13 239;]

let testhand2 = [Card.new_card Card.Clubs 1 2; 
                 Card.new_card Card.Clubs 2 11; 
                 Card.new_card Card.Clubs 3 23; 
                 Card.new_card Card.Clubs 4 41; 
                 Card.new_card Card.Clubs 10 157; 
                 Card.new_card Card.Spades 10 163; 
                 Card.new_card Card.Hearts 10 167;]

let testhand3 = [Card.new_card Card.Clubs 1 2; 
                 Card.new_card Card.Clubs 2 11; 
                 Card.new_card Card.Clubs 3 23; 
                 Card.new_card Card.Clubs 4 41; 
                 Card.new_card Card.Clubs 5 59; 
                 Card.new_card Card.Spades 5 61; 
                 Card.new_card Card.Hearts 5 67;]

let testhand4 = [Card.new_card Card.Spades 1 3; 
                 Card.new_card Card.Spades 2 13;
                 Card.new_card Card.Spades 3 29; 
                 Card.new_card Card.Spades 4 43; 
                 Card.new_card Card.Clubs 13 227; 
                 Card.new_card Card.Spades 13 229; 
                 Card.new_card Card.Diamonds 10 173;]

let testhand5 = [Card.new_card Card.Clubs 1 2; 
                 Card.new_card Card.Clubs 2 11; 
                 Card.new_card Card.Clubs 3 23; 
                 Card.new_card Card.Clubs 4 41; 
                 Card.new_card Card.Clubs 5 59; 
                 Card.new_card Card.Spades 5 61; 
                 Card.new_card Card.Diamonds 10 173;]

let  (player1 : Gamestate.player) = {name = "a"; cards = deck}

let (player2 : Gamestate.player) = {name = "b"; cards = deck2}

let tests = [
  test_draw_card "Testing removing one card" testDeck (testDeck2, card1); 
  test_draw_card "Testing removing second card" testDeck2 (testDeck3, card3); 
  test_remove "Testing removing a card from deck" testDeck card1 testDeck2;
  test_remove "Testing removing a card from deck" testDeck2 card3 testDeck3;
  test_remove "Testing removing a card from deck" testDeck3 card4 testDeck4;
  test_add_card "Testing adding a card to a deck" testDeck4 card4 testDeck3;
  test_add_card "Testing adding a card to a deck" testDeck3 card3 testDeck2;
  test_add_card "Testing adding a card to a deck" testDeck2 card1 testDeck;
  test_to_card "Testing converting a string to a card" "ace" "clubs" card1;
  test_to_card "Testing converting a string to a card" "Ace" "Clubs" card1;
  test_to_card "Testing converting a string to a card" "Ace" "diamonds" card2;
  test_to_card "Testing converting a string to a card" "king" "Spades" card5;
  test_get_suit "Testing getting suit of card Clubs" card1 Card.Clubs;
  test_get_suit "Testing getting suit of card Diamonds" card2 Card.Diamonds;
  test_get_suit "Testing getting suit of card Spades" card3 Card.Spades;
  test_get_suit "Testing getting suit of card Hearts" card4 Card.Hearts;
  test_get_rank "Testing getting rank of card Ace" card1 1;
  test_get_rank "Testing getting rank of card King" card5 13;
  test_get_rank "Testing getting rank of card 6" card6 6;
  test_get_rank "Testing getting rank of card 9" card8 9;
  test_get_prime "Testing getting prime of card" card1 2;
  test_get_prime "Testing getting prime of card" card3 3;
  test_get_prime "Testing getting prime of card" card5 229;
  test_to_string "Testing getting string of card" card1 "Ace of Clubs";
  test_to_string "Testing getting string of card" card3 "Ace of Spades";
  test_to_string "Testing getting string  of card" card5 "King of Spades";
  test_is_in "Testing whether card is in a deck" testDeck card1 true;
  test_is_in "Testing whether card is in a deck" testDeck3 card1 false;
  test_is_in "Testing whether card is in a deck" testDeck card5 false;
  test_is_in "Testing whether card is in a deck" testDeck3 card4 true;
  test_deck_to_string "Testing deck to string function" testDeck "Ace of Clubs; Ace of Spades; Ace of Hearts; Ace of Diamonds";
  test_deck_to_string "Testing deck to string function" testDeck2 "Ace of Spades; Ace of Hearts; Ace of Diamonds";
  test_deck_to_string "Testing deck to string function" testDeck3 "Ace of Hearts; Ace of Diamonds";
  test_check_win "Testing giving winning hand returns true" testhand1 true;
  test_check_win "Testing giving dif winning hand returns true" testhand2 true;
  test_check_win "Testing giving wining hand returns true 3" testhand3 true;
  test_check_win "Testing giving losing hand returns false" testhand4 false;
  test_check_win "Testing giving losing hand returns false" testhand5 false;
  test_draw_from_deck "Testing drawing from a deck" gamestate (deck4, card6);
  test_draw_from_deck "Testing drawing from a deck" gamestate2 (deck5, card7);
  test_get_num_players "Testing getting number of players from gamestate" gamestate 3;
  test_get_num_players "Testing getting number of players from gamestate" gamestate3 5;
  test_get_name "Testing getting name of a player" player1 "a";
  test_get_name "Testing getting name of a player" player2 "b";
  test_get_hand "Testing getting hand of a player" player1 deck;
  test_get_hand "Testing getting hand of a player" player2 deck2;
  test_get_deck "testing getting deck from gamestate" gamestate2 deck4;
  test_display_hand "Testing displaying a test hand prints correctly"
    (Deck.create_deck testhand1) ("* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *
*  * * *          **  * * *          **  * * *          **  *   *          **  *   *          **  *   *          **  *   *          *
*  *   *          **      *          **      *          **  *   *          **  * *            **  * *            **  * *            *
*  * * *          **  * * *          **  * * *          **  * * *          **  *              **  *              **  *              *
*  *   *          **  *              **      *          **      *          **  * *            **  * *            **  * *            *
*  *   *          **  * * *          **  * * *          **      *          **  *   *          **  *   *          **  *   *          *
*          *      **          *      **          *      **          *      **        * * *    **          *      **          *      *
*        * * *    **        * * *    **        * * *    **        * * *    **      * * * * *  **        * * *    **        * * *    *
*      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  *
*          *      **          *      **          *      **          *      **          *      **          *      **        * * *    *
*        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **          *      *
* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *" ^ "\n");
  test_display_hand "Testing displaying a different test hand prints correctly"
    (Deck.create_deck testhand2) ("* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *
*  * * *          **  * * *          **  * * *          **  *   *          **  *  ***         **  *  ***         **  *  ***         *
*  *   *          **      *          **      *          **  *   *          **  *  * *         **  *  * *         **  *  * *         *
*  * * *          **  * * *          **  * * *          **  * * *          **  *  * *         **  *  * *         **  *  * *         *
*  *   *          **  *              **      *          **      *          **  *  * *         **  *  * *         **  *  * *         *
*  *   *          **  * * *          **  * * *          **      *          **  *  ***         **  *  ***         **  *  ***         *
*        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **          *      **        *   *    *
*      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **        * * *    **      * * * * *  *
*      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **        * * *    *
*          *      **          *      **          *      **          *      **          *      **          *      **          *      *
*        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **                 *
* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *" ^ "\n");
  test_display_hand "Testing displaying third hand prints correctly" 
    (Deck.create_deck testhand3)
    ("* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *
*  * * *          **  * * *          **  * * *          **  *   *          **  * * *          **  * * *          **  * * *          *
*  *   *          **      *          **      *          **  *   *          **  *              **  *              **  *              *
*  * * *          **  * * *          **  * * *          **  * * *          **  * * *          **  * * *          **  * * *          *
*  *   *          **  *              **      *          **      *          **      *          **      *          **      *          *
*  *   *          **  * * *          **  * * *          **      *          **  * * *          **  * * *          **  * * *          *
*        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **          *      **        *   *    *
*      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **        * * *    **      * * * * *  *
*      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **      * * * * *  **        * * *    *
*          *      **          *      **          *      **          *      **          *      **          *      **          *      *
*        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **        * * *    **                 *
* * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * ** * * * * * * * * *" ^ "\n");
]

let suite = "suite" >::: tests



let _ = run_test_tt_main suite