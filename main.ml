open Command
open Gamestate
open Deck
open Card
open Discard

type game = {
    mutable gmst :  Gamestate.g
} 

let rec main game temp =
    let numpl = Gamestate.get_num_players game.gmst in
        let curplayer = List.nth (Gamestate.get_players game.gmst) (temp mod numpl) in
            ANSITerminal.(print_string [red]
                  (String.concat "" ["\n\n"; "It is currently "; (Gamestate.get_name curplayer); "'s turn.\n\n"]));
            ANSITerminal.(print_string [green] "This is your hand:\n\n");
            print_string (Deck.display_hand (Gamestate.get_hand curplayer));
            print_string "\n\n";
            ANSITerminal.(print_string [green] (Deck.deck_to_string (Gamestate.get_hand curplayer))); 
            ANSITerminal.(print_string [yellow] (String.concat "" ["\n\n"; "The top card in the discard is currently: "; Card.to_string (Discard.peek (Gamestate.get_discard game.gmst)) ]));
            print_string "\n\n";
            ANSITerminal.(print_string [blue] (String.concat "" [(Gamestate.get_name curplayer); " enter either take from discard, take from deck, pass, or gin.\n"]));
            ANSITerminal.(print_string [red]  "> ");
            let str = read_line () in
                match Command.parse str with
                | TakeDiscard -> if (Discard.is_empty (Gamestate.get_discard game.gmst)) then main_helper game temp else take_discard_helper game curplayer temp 
                | TakeStock -> take_stock_helper game curplayer temp
                | Pass -> main game (temp+1)
                | Gin -> if (check_win (Deck.deck_to_list (Gamestate.get_hand curplayer))) then gin game curplayer else failed_gin game temp
                | Quit -> ()
                | a -> main_helper game temp

and main_helper game temp =
    ANSITerminal.(print_string [blue] "That was an invalid input. Please enter either take from discard, take from deck, pass, or gin.\n");
    ANSITerminal.(print_string [red]  "> ");
    let numpl = Gamestate.get_num_players game.gmst in
        let curplayer = List.nth (Gamestate.get_players game.gmst) (temp mod numpl) in
            let str = read_line () in
                match Command.parse str with
                | TakeDiscard -> if (Discard.is_empty (Gamestate.get_discard game.gmst)) then main_helper game temp else take_discard_helper game curplayer temp 
                | TakeStock -> take_stock_helper game curplayer temp
                | Pass -> main game (temp+1)
                | Gin -> if (check_win (Deck.deck_to_list (Gamestate.get_hand curplayer))) then gin game curplayer else failed_gin game temp
                | Quit -> ()
                | a -> main_helper game temp

and failed_gin game temp =
    ANSITerminal.(print_string [blue] "You do not have gin. Please enter either take from discard, take from deck, or pass.\n");
    ANSITerminal.(print_string [red]  "> ");
    let numpl = Gamestate.get_num_players game.gmst in
        let curplayer = List.nth (Gamestate.get_players game.gmst) (temp mod numpl) in
            let str = read_line () in
                match Command.parse str with
                | TakeDiscard -> if (Discard.is_empty (Gamestate.get_discard game.gmst)) then main_helper game temp else take_discard_helper game curplayer temp 
                | TakeStock -> take_stock_helper game curplayer temp
                | Pass -> draw_check game (temp+1)
                | Gin -> if (check_win (Deck.deck_to_list (Gamestate.get_hand curplayer))) then gin game curplayer else failed_gin game temp
                | Quit -> ()
                | a -> main_helper game temp

and take_discard_helper gm plyr temp =
    draw_card gm plyr (Gamestate.draw_from_discard gm.gmst) temp; Gamestate.remove_from_discard gm.gmst; draw_check gm (temp+1)

and take_stock_helper gm plyr temp =
    let (d,c) = (Gamestate.draw_from_deck gm.gmst) in 
        draw_card gm plyr c temp; Gamestate.update_deck gm.gmst d; draw_check gm (temp+1)

(** [play_game str] starts the game. *)
and start_game objphr gm =
        gm.gmst <- (new_game objphr (deck_to_list (shuffle (shuffle new_deck)))); Gamestate.start_discard gm.gmst; main gm 0

and shuffle dk =
    let temp = Random.int 15 in 
        if (temp == 7) then
            Deck.shuffle_deck dk
        else 
            Deck.shuffle_deck (shuffle dk)

and gin game plyr =
    ANSITerminal.(print_string [red]
                  (String.concat "" ["\n\n"; (Gamestate.get_name plyr); "'is the winner because they got gin. \n\n"]));
            ANSITerminal.(print_string [blue] "Either enter new game followed by a list of player names (between 2 and 5) or enter quit to exit. \n\n");
            print_string "\n\n";
            ANSITerminal.(print_string [red]  "> ");
            let gmstate = Gamestate.new_empty_game () in 
                let gm = {gmst = gmstate} in 
                    let str = read_line () in 
                        match Command.parse str with
                        | NewGame [h1; h2] -> start_game [h1; h2] gm
                        | NewGame [h1; h2; h3] -> start_game [h1; h2; h3] gm
                        | NewGame [h1; h2; h3; h4] -> start_game [h1; h2; h3; h4] gm
                        | NewGame [h1; h2; h3; h4; h5] -> start_game [h1; h2; h3; h4; h5] gm
                        | Quit -> ()
                        | s -> startpass ()

and startpass () =
    ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Gin Rummy engine.\n");
    ANSITerminal.(print_string [blue] "That is not a valid new game. Please enter new game followed by names of players (between 2 and 5) to begin.\n");
    ANSITerminal.(print_string [red]  "> ");
    let gmstate = Gamestate.new_empty_game () in 
        let gm = {gmst = gmstate} in 
            let str = read_line () in 
                match Command.parse str with
                | NewGame [h1; h2] -> start_game [h1; h2] gm
                | NewGame [h1; h2; h3] -> start_game [h1; h2; h3] gm
                | NewGame [h1; h2; h3; h4] -> start_game [h1; h2; h3; h4] gm
                | NewGame [h1; h2; h3; h4; h5] -> start_game [h1; h2; h3; h4; h5] gm
                | Quit -> ()
                | s -> startpass ()

and draw_card gm (plyr : player) (card : Card.c) temp : unit =
    let card_string = Card.to_string card in
    ANSITerminal.(print_string [yellow] (String.concat "" ["\n\n"; "The card you drew is:   "; card_string; "\n\n"]));
    print_string "\n\n";
    ANSITerminal.(print_string [green] "This is your hand:   ");
    ANSITerminal.(print_string [green] (Deck.deck_to_string (Gamestate.get_hand plyr))); 
    print_string "\n\n";
    ANSITerminal.(print_string [blue] "Please enter drop followed by the card you wish to discard. \n");
    ANSITerminal.(print_string [red]  "> ");
    let str = read_line () in
        match Command.parse str with 
        | Drop [h1; h2; h3] -> (match (to_card h1 h3) with 
                                | c -> if (Deck.is_in (Gamestate.get_hand plyr) c) then discard_helper gm plyr card c temp else if ((Card.get_prime c)==(Card.get_prime card)) then discard_helper2 gm card temp else draw_card_helper gm plyr card temp)
        | Quit -> ()
        | s -> draw_card_helper gm plyr card temp

and draw_card_helper gm (plyr : player) (card : Card.c) temp : unit =
    let card_string = Card.to_string card in
    ANSITerminal.(print_string [yellow] (String.concat "" ["\n\n"; "The card you drew is:   "; card_string; "\n\n"]));
    print_string "\n\n";
    ANSITerminal.(print_string [green] "This is your hand:   ");
    ANSITerminal.(print_string [green] (Deck.deck_to_string (Gamestate.get_hand plyr))); 
    print_string "\n\n";
    ANSITerminal.(print_string [blue] "That is not a valid card. Please enter the card you wish to discard. \n");
    ANSITerminal.(print_string [red]  "> ");
    let str = read_line () in
        match Command.parse str with 
        | Drop [h1; h2; h3] -> (match (to_card h1 h3) with 
                                | c -> if (Deck.is_in (Gamestate.get_hand plyr) c) then discard_helper gm plyr card c temp else if ((Card.get_prime c)==(Card.get_prime card)) then discard_helper2 gm card temp else draw_card_helper gm plyr card temp)
        | Quit -> ()
        | s -> draw_card_helper gm plyr card temp

and discard_helper gm plyr card1 card2 temp =  
    Gamestate.discard gm.gmst (Gamestate.get_name plyr) card2; Gamestate.add_card gm.gmst (Gamestate.get_name plyr) card1

and discard_helper2 gm card temp =
     Gamestate.discard2 gm.gmst card

and draw_check game temp =
    let decksize = List.length (Deck.deck_to_list (Gamestate.get_deck game.gmst)) in 
        if (decksize > 2) then 
            main game temp
        else draw ()

and draw () =  
    ANSITerminal.(print_string [red]
                  "This game ended in a draw because there were 2 or less cards left in the deck.\n\n");
            ANSITerminal.(print_string [blue] "Either enter new game followed by a list of player names (between 2 and 5) or enter quit to exit. \n\n");
            print_string "\n\n";
            ANSITerminal.(print_string [red]  "> ");
            let gmstate = Gamestate.new_empty_game () in 
                let gm = {gmst = gmstate} in 
                    let str = read_line () in 
                        match Command.parse str with
                        | NewGame [h1; h2] -> start_game [h1; h2] gm
                        | NewGame [h1; h2; h3] -> start_game [h1; h2; h3] gm
                        | NewGame [h1; h2; h3; h4] -> start_game [h1; h2; h3; h4] gm
                        | NewGame [h1; h2; h3; h4; h5] -> start_game [h1; h2; h3; h4; h5] gm
                        | Quit -> ()
                        | s -> startpass ()

(** [main ()] prompts for the game to play, then starts it. *)
let start () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Gin Rummy engine.\n");
  ANSITerminal.(print_string [blue] "Please enter new game followed by names of players (between 2 and 5) to begin.\n");
  ANSITerminal.(print_string [red]  "> ");
  let gmstate = Gamestate.new_empty_game () in 
    let gm = {gmst = gmstate} in 
        let str = read_line () in 
            match Command.parse str with
            | NewGame [h1; h2] -> start_game [h1; h2] gm
            | NewGame [h1; h2; h3] -> start_game [h1; h2; h3] gm
            | NewGame [h1; h2; h3; h4] -> start_game [h1; h2; h3; h4] gm
            | NewGame [h1; h2; h3; h4; h5] -> start_game [h1; h2; h3; h4; h5] gm
            | Quit -> ()
            | s -> startpass ()

(* Execute the game engine. *)
let () = start ()