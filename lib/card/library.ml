open BaseTypes
open Engine
open Action
open Event

let novice =
  {
    name = "Novice Engineer";
    health = 1;
    attack = 1;
    cost = 2;
    tribes = [ "Gnome" ];
    triggers = [ (battlecry, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let card_drawer =
  {
    name = "Card Drawer";
    health = 7;
    attack = 7;
    cost = 4;
    tribes = [ "Demon" ];
    triggers = [ (card_drawn &@ on_board, Instant (end_turn 0)) ];
    card_type = Minion;
  }

let emerald_reaver =
  {
    name = "Emerald Reaver";
    health = 1;
    attack = 2;
    cost = 1;
    tribes = [ "Beast" ];
    triggers =
      [ (battlecry, deal_damage 1 (Player 0) <*> deal_damage 1 (Player 1)) ];
    card_type = Minion;
  }

let elemental_watcher =
  {
    name = "Elemental Watcher";
    health = 2;
    attack = 1;
    cost = 1;
    tribes = [ "Elemental" ];
    triggers =
      [
        (battlecry, enter_board);
        (battlecry &@ played_elemental_lastturn, Instant (draw_card 0));
        (end_own_turn, pass_turn_elemental);
        ( (card_type_played "Elemental") &@ (!@ battlecry),
          played_elemental );
      ];
    card_type = Minion;
  }

let auctioneer =
  {
    name = "Gadgetzan Auctioneer";
    health = 4;
    attack = 4;
    cost = 6;
    tribes = [ "Goblin" ];
    triggers = [ (you_spell_cast, Instant (draw_card 0)) ];
    card_type = Minion;
  }

let arcane_intelect =
  {
    name = "Arcane Intelect";
    health = 0;
    attack = 0;
    cost = 3;
    tribes = [ "Arcane" ];
    triggers = [ (battlecry, Instant (draw_card 0) <*> Instant (draw_card 0)) ];
    card_type = Spell;
  }

let fireball =
  {
    name = "Fireball";
    health = 0;
    attack = 0;
    cost = 4;
    tribes = [ "Fire" ];
    triggers = [ (battlecry, Targetted (deal_damage 6))];
    card_type = Spell;
  }
