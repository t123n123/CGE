type event = CardPlayed of card_instance | CardDeath of card_instance

and card = {
  name : string;
  health : int;
  attack : int;
  cost : int;
  triggers : ((event -> card_instance -> bool) * (gamestate -> gamestate)) list;
}

and playerstate = {
  hp : int;
  mana : int;
  max_mana : int;
  player_id : int;
  deck : card_instance list;
  hand : card_instance list;
  board : card_instance list;
}

and card_instance = { card : card; owner : int; card_id : int }
and gamestate = { players : playerstate list }

let string_of_card (card : card_instance) : string =
  card.card.name ^ " - Cost: "
  ^ string_of_int card.card.cost
  ^ " Stats: "
  ^ string_of_int card.card.attack
  ^ "/"
  ^ string_of_int card.card.health

let string_of_cardlist (cards : card_instance list) : string =
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

let string_of_players (players : playerstate list) : string =
  List.fold_left
    (fun x y -> x ^ "\n" ^ y)
    ""
    (List.map string_of_playerstate players)

let rec replace list index new_value =
  if index == 0 then new_value :: List.tl list
  else List.hd list :: replace (List.tl list) (index - 1) new_value

let rec remove list index =
  if index == 0 then List.tl list
  else List.hd list :: remove (List.tl list) (index - 1)

let draw_card (player_nr : int) (players : gamestate) : gamestate =
  let player = List.nth players.players player_nr in
  let new_deck = List.tl player.deck in
  let new_hand = List.hd player.deck :: player.hand in
  {
    players =
      replace players.players player_nr
        { player with deck = new_deck; hand = new_hand };
  }

let check_event event gamestate = gamestate

let play_card (player_nr : int) (card_nr : int) (game : gamestate) : gamestate =
  let player = List.nth game.players player_nr in
  let card = List.nth player.hand card_nr in
  let new_hand = remove player.hand card_nr in
  let new_board = card :: player.board in
  let new_game =
    {
      players =
        replace game.players player_nr
          {
            player with
            hand = new_hand;
            board = new_board;
            mana = player.mana - card.card.cost;
          };
    }
  in
  let event = CardPlayed card in
  check_event event new_game

(* card.battlecry
   (replace players player_nr
      { player with hand = new_hand; board = new_board }) *)

let end_turn (game : gamestate) : gamestate =
  { players = List.tl game.players @ [ List.hd game.players ] }

let battlecry ev c : bool =
  match ev with
  | CardPlayed card -> if card.card_id == c.card_id then true else false
  | _ -> false

let novice =
  {
    name = "Novice Engineer";
    health = 1;
    attack = 1;
    cost = 2;
    triggers = [ (battlecry, draw_card 0) ];
  }

let a = { card = novice; owner = 0; card_id = 0 }

let auctioneer =
  {
    name = "Gadgetzan Auctioneer";
    health = 4;
    attack = 4;
    cost = 6;
    triggers = [];
  }

let b = { card = auctioneer; owner = 0; card_id = 1 }

let game =
  [
    {
      player_id = 0;
      hp = 30;
      mana = 0;
      max_mana = 0;
      deck = [ b ];
      hand = [ a ];
      board = [ b ];
    };
    {
      player_id = 1;
      hp = 30;
      mana = 0;
      max_mana = 0;
      deck = [ b ];
      hand = [ a ];
      board = [ b ];
    };
  ]

(* let game2 = play_card 0 0 (end_turn (play_card 0 0 game))

let () =
  let _ = print_endline (string_of_players game) in
  print_endline (string_of_players game2) *)
