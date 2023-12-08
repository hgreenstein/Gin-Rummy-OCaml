open Deck
open Discard

type player = {
  name : string;
  cards : Card.c Deck.d;
}

type g = {
  mutable dk: Card.c Deck.d;
  mutable discard: Card.c Discard.p;
  mutable players: player list;
}

let get_deck gm =
  gm.dk

let get_discard gm =
  gm.discard

let get_hand plyr =
    plyr.cards

let get_name plyr =
    plyr.name

let get_players gmst =
    gmst.players

let get_num_players gmst = 
    List.length gmst.players

let new_player str = 
  {name = str; cards = Deck.new_empty_deck}

let rec remove_x lst x count = 
  match lst with
  | [] -> lst
  | h::t -> if count < x then (remove_x t x (count + 1)) else lst

let deal deck playerlst =
  if (List.length playerlst = 2) then
    [{name = List.nth playerlst 0; cards = Deck.create_deck [(List.nth deck 0);(List.nth deck 1);
      (List.nth deck 2);(List.nth deck 3);
      (List.nth deck 4);(List.nth deck 5);
      (List.nth deck 6)]}; 
     {name = List.nth playerlst 1; cards = Deck.create_deck [(List.nth deck 7);(List.nth deck 8);
      (List.nth deck 9);(List.nth deck 10);
      (List.nth deck 11);(List.nth deck 12);
      (List.nth deck 13)]}]
  else if (List.length playerlst = 3) then
    [{name = List.nth playerlst 0; cards = Deck.create_deck [(List.nth deck 0);(List.nth deck 1);
      (List.nth deck 2);(List.nth deck 3);
      (List.nth deck 4);(List.nth deck 5);
      (List.nth deck 6)]}; 
    {name = List.nth playerlst 1; cards = Deck.create_deck [(List.nth deck 7);(List.nth deck 8);
      (List.nth deck 9);(List.nth deck 10);
      (List.nth deck 11);(List.nth deck 12);
      (List.nth deck 13)]};
     {name = List.nth playerlst 2; cards = Deck.create_deck [(List.nth deck 14);(List.nth deck 15);
      (List.nth deck 16);(List.nth deck 17);
      (List.nth deck 18);(List.nth deck 19);
      (List.nth deck 20)]}]
  else if (List.length playerlst = 4) then
    [{name = List.nth playerlst 0; cards = Deck.create_deck [(List.nth deck 0);(List.nth deck 1);
      (List.nth deck 2);(List.nth deck 3);
      (List.nth deck 4);(List.nth deck 5);
      (List.nth deck 6)]}; 
     {name = List.nth playerlst 1; cards = Deck.create_deck [(List.nth deck 7);(List.nth deck 8);
      (List.nth deck 9);(List.nth deck 10);
      (List.nth deck 11);(List.nth deck 12);
      (List.nth deck 13)]};
     {name = List.nth playerlst 2; cards = Deck.create_deck [(List.nth deck 14);(List.nth deck 15);
      (List.nth deck 16);(List.nth deck 17);
      (List.nth deck 18);(List.nth deck 19);
      (List.nth deck 20)]};
     {name = List.nth playerlst 3; cards = Deck.create_deck [(List.nth deck 21);(List.nth deck 22);
      (List.nth deck 23);(List.nth deck 24);
      (List.nth deck 25);(List.nth deck 26);
      (List.nth deck 27)]}]
  else
    [{name = List.nth playerlst 0; cards = Deck.create_deck [(List.nth deck 0);(List.nth deck 1);
      (List.nth deck 2);(List.nth deck 3);
      (List.nth deck 4);(List.nth deck 5);
      (List.nth deck 6)]}; 
     {name = List.nth playerlst 1; cards = Deck.create_deck [(List.nth deck 7);(List.nth deck 8);
      (List.nth deck 9);(List.nth deck 10);
      (List.nth deck 11);(List.nth deck 12);
      (List.nth deck 13)]};
     {name = List.nth playerlst 2; cards = Deck.create_deck [(List.nth deck 14);(List.nth deck 15);
      (List.nth deck 16);(List.nth deck 17);
      (List.nth deck 18);(List.nth deck 19);
      (List.nth deck 20)]};
     {name = List.nth playerlst 3; cards = Deck.create_deck [(List.nth deck 21);(List.nth deck 22);
      (List.nth deck 23);(List.nth deck 24);
      (List.nth deck 25);(List.nth deck 26);
      (List.nth deck 27)]};
     {name = List.nth playerlst 4; cards = Deck.create_deck [(List.nth deck 28);(List.nth deck 29);
      (List.nth deck 30);(List.nth deck 31);
      (List.nth deck 32);(List.nth deck 33);
      (List.nth deck 34)]}]

let new_game namelst deck = 
   if (List.length namelst > 5 || List.length namelst <= 1) then 
   failwith ("Invalid Number of Players")
   else 
   {dk = Deck.create_deck (remove_x deck (List.length namelst * 7) 0);
   discard = Discard.empty (); 
   players = deal deck namelst;}

let rec discard_helper (plyrs : player list) plyr dcard = 
  match plyrs with 
  | h::t -> (if h.name == plyr then
               {name = plyr; cards = Deck.remove h.cards dcard} :: t
             else h :: discard_helper t plyr dcard)
  | [] ->  failwith("This player does not have this card.")

let discard (gmst : g) plyr dcard = 
  let plyrs = gmst.players in
  gmst.players <- discard_helper plyrs plyr dcard;
  Discard.push dcard gmst.discard

let discard2 gmst card =
    Discard.push card gmst.discard

let rec draw_from_deck_helper (plyrs : player list) plyr (card : Card.c) = 
  match plyrs with 
  | h::t -> if h.name == plyr then ({name = plyr; cards = Deck.add_card h.cards card} :: t) 
    else h :: draw_from_deck_helper t plyr card
  | [] ->  failwith("This player does not have this card.")

let add_card gmst plyr card = 
  gmst.players <- draw_from_deck_helper gmst.players plyr card

let draw_from_deck (gmst : g) = 
  let deck = gmst.dk in 
    Deck.draw_card deck

let draw_from_discard (gmst : g) = 
  let discard = gmst.discard in 
      Discard.peek discard

let remove_from_discard (gmst : g) =
  Discard.pop gmst.discard

let update_deck (gmst : g) deck =
  gmst.dk <- deck

(**let new_game numplayers deck = 
   if (numplayers > 5 || numplayers <= 1) then 
   failwith ("Invalid Number of Players")
   else 
   {dk = remove_x deck (numplayers * 7) 0;
   discard = Discard.empty (); 
   players = deal deck numplayers;}
*)

let new_empty_game () =
  let dis = Discard.empty () in
    {dk = (Deck.create_deck []); discard = dis; players = []}

let start_discard gmst = 
    match Deck.draw_card gmst.dk with 
    | (d,c) -> Discard.push c gmst.discard; gmst.dk <- d