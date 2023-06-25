open CGE.BaseTypes
open CGE.Engine
open CGE.Action
open CGE.Event

let novice =
  {
    name = "Novice Engineer";
    health = 1;
    attack = 1;
    cost = 2;
    tribes = ["Gnome"];
    triggers = [ (battlecry, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let card_drawer =
  {
    name = "Card Drawer";
    health = 7;
    attack = 7;
    cost = 4;
    tribes = ["Demon"];
    triggers = [ (check_and card_drawn on_board , Instant (end_turn 0)) ];
    card_type = Minion;
  }

let emerald_reaver = 
  {
    name = "Emerald Reaver";
    health = 1;
    attack = 2;
    cost = 1;
    tribes = ["Beast"];
    triggers = [ battlecry, ((deal_damage (1) (Player 0)) <*> (deal_damage (1) (Player 1)))];
    card_type = Minion;
  }

let elemental_watcher =
  {
    name = "Elemental Watcher";
    health = 2;
    attack = 1;
    cost = 1;
    tribes = ["Elemental"];
    triggers = [ (battlecry , enter_board);
      (check_and battlecry played_elemental_lastturn, Instant (draw_card 0));
      (end_own_turn, pass_turn_elemental);
      ((check_and(card_type_played "Elemental") (check_not (battlecry))), played_elemental)];
    card_type = Minion;
  }



let drawer = { card = card_drawer; owner = 0; card_id = 0; card_counters = Counter.empty}
let a = { card = novice; owner = 0; card_id = 0; card_counters = Counter.empty}
let elem_watcher = {card = elemental_watcher; owner = 0; card_id = 3; card_counters = Counter.empty}
let emer_reaver = {card = emerald_reaver; owner = 0; card_id = 7; card_counters = Counter.empty}

let auctioneer =
  {
    name = "Gadgetzan Auctioneer";
    health = 4;
    attack = 4;
    cost = 6;
    tribes = ["Goblin"];
    triggers = [ (you_spell_cast, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let arcane_intelect =
  {
    name = "Arcane Intelect";
    health = 0;
    attack = 0;
    cost = 3;
    tribes = ["Arcane"];
    triggers = [ (battlecry, Instant (draw_card 0) <*> Instant (draw_card 0)) ];
    card_type = Spell;
  }

let c = { card = arcane_intelect; owner = 0; card_id = 2 ; card_counters = Counter.empty}
let b = { card = auctioneer; owner = 0; card_id = 1 ; card_counters = Counter.empty}
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
          deck = [ref c];
          hand = [ref emer_reaver; ref elem_watcher];
          board = [];
        };
        {
          player_id = 1;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [ ];
          hand = [ ];
          board = [ ];
        };
      ];
    current_player = 0;
  }




let () =
  let _ = print_endline @@ string_of_gamestate game in
  let game1 = (play_card 0 0 game) in
  let _ = print_endline @@ string_of_gamestate (game1) in 
  let game2 = lazy (play_card 0 0 (end_turn 1 (end_turn 0 game1))) in
  print_endline @@ string_of_gamestate (Lazy.force game2)
