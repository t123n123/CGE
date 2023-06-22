type card = {
  name : string;
  health : int;
  attack : int;
  cost : int;
  battlecry : playerstate list -> playerstate list;
}

and playerstate = {
  hp : int;
  mana : int;
  max_mana : int;
  deck : card list;
  hand : card list;
  board : card list;
}

let string_of_card (card : card) : string =
  card.name ^ " - Cost: " ^ string_of_int card.cost ^ " Stats: "
  ^ string_of_int card.attack ^ "/" ^ string_of_int card.health

let string_of_cardlist (cards : card list) : string =
  List.fold_left
    (fun left right -> left ^ right)
    ""
    (List.map (fun card -> "\n" ^ string_of_card card) cards)

let string_of_playerstate (player : playerstate) : string =
  "Player : \n" ^ "HP: " ^ string_of_int player.hp ^ "\n" ^ "Hand: "
  ^ string_of_cardlist player.hand
  ^ "\n\n" ^ "Deck: "
  ^ string_of_cardlist player.deck
  ^ "\n\n" ^ "Board: "
  ^ string_of_cardlist player.board
  ^ "\n\n"

let rec replace list index new_value =
  if index == 0 then new_value :: List.tl list
  else List.hd list :: replace (List.tl list) (index - 1) new_value

let rec remove list index = 
  if index == 0 then List.tl list 
  else List.hd list :: remove (List.tl list) (index - 1)


let draw_card (player_nr : int) (players : playerstate list) : playerstate list
    =
  let player = List.nth players player_nr in
  let new_deck = List.tl player.deck in
  let new_hand = List.hd player.deck :: player.hand in
  replace players player_nr { player with deck = new_deck; hand = new_hand }

let play_card (player_nr : int) (card_nr : int) (players : playerstate list) : playerstate list 
    = 
  let player = List.nth players player_nr in
  let card = List.nth player.hand card_nr in 
  let new_hand = remove player.hand card_nr in
  let new_board = card :: player.board in 
  card.battlecry (replace players player_nr { player with hand = new_hand; board = new_board})

let a =
  {
    name = "Novice Engineer";
    health = 1;
    attack = 1;
    cost = 2;
    battlecry = draw_card 0;
  }
let b = 
  {
    name = "Gadgetzan Auctioneer";
    health = 4;
    attack = 4;
    cost = 6;
    battlecry = fun x -> x;
  }

let game =
  [
    {
      hp = 30;
      mana = 0;
      max_mana = 0;
      deck = [ b ];
      hand = [ a ];
      board = [ b ];
    };
  ]

let game2 = play_card 0 0 game  
let () = let _ = print_endline(string_of_playerstate (List.hd game)) in print_endline (string_of_playerstate (List.hd game2))
