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
    triggers = [ (card_drawn, Instant (end_turn 0)) ];
    card_type = Minion;
  }

let elemental_watcher =
  {
    name = "Elemental Watcher";
    health = 2;
    attack = 1;
    cost = 1;
    tribes = ["Elemental"];
    triggers = [ ((fun ev c -> (card_type_played "Elemental" ev c) && not (battlecry ev c)), Instant (end_turn 0)) ];
    card_type = Minion;
  }


let drawer = { card = card_drawer; owner = 0; card_id = 0; card_counters = Counter.empty}
let a = { card = novice; owner = 0; card_id = 0; card_counters = Counter.empty}
let elem_watcher = {card = elemental_watcher; owner = 0; card_id = 3; card_counters = Counter.empty}

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
          deck = [ b; b; b ];
          hand = [ elem_watcher; {elem_watcher with card_id = 4}; a ];
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

let game2 = lazy (play_card 0 0 (play_card 0 0 game))

let () =
  let _ = print_endline @@ string_of_gamestate game in
  print_endline @@ string_of_gamestate (Lazy.force game2)
