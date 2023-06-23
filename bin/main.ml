(* type card = {
    name : string;
    stats : stats
    triggers (event, f)
   } *)

(* type action = Instant of (gamestate -> gamestate) | Targetted of (target -> action)

   let play_card target gamestate : action=
     Instant (fun gamestate -> let card = find_card gamestate in
     put_card_on_field @@ remove_card_from_hand gamestate)


   let do_something (f:action) gamestate : gamestate =
     check_events
     match f with
      Instant f -> _
      b -> _
     check_events


   let rec process_turn gamestate =
     let action = input in
     let game = do_something action gamestate in
     process_turn game
*)

type event =
  | CardPlayed of card_instance
  | CardDeath of card_instance
  | CardDraw of card_instance

and card_type = Spell | Minion

and card = {
  name : string;
  health : int;
  attack : int;
  cost : int;
  triggers : trigger list;
  card_type : card_type;
}

and target = Card of card_instance | Player of int

and action =
  | Instant of (gamestate -> gamestate)
  | Targetted of (target -> action)

and trigger = (event -> card_instance -> bool) * action

and playerstate = {
  hp : int;
  mana : int;
  max_mana : int;
  player_id : int;
  deck : card_instance list;
  hand : card_instance list;
  board : card_instance list;
}

and card_instance = {
  card : card;
  owner : int;
  card_id : int;
  triggers : trigger list;
}

and gamestate = { players : playerstate list; current_player: int}

let ( << ) f g x = f (g x)

let rec seq a1 a2 =
  match (a1, a2) with
  | Instant f1, Instant f2 -> Instant (f1 << f2)
  | Targetted f1, _ -> Targetted (fun action -> seq (f1 action) a2)
  | Instant f1, Targetted f2 -> Targetted (fun action -> seq a1 (f2 action))

let rec apply_action act state targets =
  match act with
  | Instant f -> f state
  | Targetted f -> apply_action (f (List.hd targets)) state (List.tl targets)

let get_player_triggers player =
  List.fold_left ( @ ) []
    (List.map
       (fun x -> List.map (fun y -> (x, y)) x.card.triggers)
       player.board)

let get_all_triggers state =
  List.fold_left (fun ls pl -> ls @ get_player_triggers pl) [] state.players

let rec process_event event state =
  let triggers = get_all_triggers state in
  List.fold_left
    (fun state trigger ->
      if (fst @@ snd trigger) event (fst trigger) then
        apply_action (snd @@ snd trigger) state []
      else state)
    state triggers

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
  let state =
    { players with 
      players =
        replace players.players player_nr
          { player with deck = new_deck; hand = new_hand };
    }
  in
  let event = CardDraw (List.hd player.deck) in
  process_event event state

let play_card (player_nr : int) (card_nr : int) (game : gamestate) : gamestate =
  let player = List.nth game.players player_nr in
  let card = List.nth player.hand card_nr in
  let new_hand = remove player.hand card_nr in
  let new_board = card :: player.board in
  let new_game =
    { game with 
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
  process_event event new_game

(* card.battlecry
   (replace players player_nr
      { player with hand = new_hand; board = new_board }) *)

let end_turn (game : gamestate) : gamestate = let _ = print_endline ("Turn Ended") in game


let battlecry ev c : bool =
  match ev with CardPlayed card -> card.card_id == c.card_id | _ -> false

let spell_cast ev c : bool =
  match ev with CardPlayed card -> card.card.card_type = Spell | _ -> false

let you_spell_cast ev c : bool =
  match ev with
  | CardPlayed card -> card.card.card_type = Spell && card.owner = c.owner
  | _ -> false

let card_drawn ev c : bool =
  match ev with CardDraw card -> card.owner = c.owner | _ -> false

let novice =
  {
    name = "Novice Engineer";
    health = 1;
    attack = 1;
    cost = 2;
    triggers = [ (battlecry, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let card_drawer =
  {
    name = "Card Drawer";
    health = 7;
    attack = 7;
    cost = 4;
    triggers = [ (card_drawn, Instant end_turn) ];
    card_type = Minion;
  }

let drawer = { card = card_drawer; owner = 0; card_id = 10; triggers = [] }
let a = { card = novice; owner = 0; card_id = 0; triggers = [] }

let auctioneer =
  {
    name = "Gadgetzan Auctioneer";
    health = 4;
    attack = 4;
    cost = 6;
    triggers = [ (you_spell_cast, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let arcane_intelect =
  {
    name = "Arcane Intelect";
    health = 0;
    attack = 0;
    cost = 3;
    triggers =
      [ (battlecry, seq (Instant (draw_card 0)) (Instant (draw_card 0))) ];
    card_type = Spell;
  }

let c = { card = arcane_intelect; owner = 0; card_id = 2; triggers = [] }
let b = { card = auctioneer; owner = 0; card_id = 1; triggers = [] }
let b2 = { b with owner = 1 }

let game =
  {
    players =
      [
        {
          player_id = 0;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [ b; b; b ];
          hand = [ c ];
          board = [ drawer ];
        };
        {
          player_id = 1;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [ b2 ];
          hand = [ a ];
          board = [ b2 ];
        };
      ];
      current_player = 0;
  }

let game2 = play_card 0 0 game

let () =
  let _ = print_endline (string_of_players game.players) in
  print_endline (string_of_players game2.players)
