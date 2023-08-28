open CGE.BaseTypes
open CGE.Engine
open CGE.Action
open CGE.Event
open CGE
open Utils

let hearthstone_rules : CGE.Rules.t =
  {
    card_draw_rules =
      { starting_hand_size = 3; max_hand_size = 10; amount_drawn = 1 };
    hp_rules = Some { starting_hp = 30; hp_per_turn = 0 };
    mana_rules = Some { starting_mana = 0; mana_per_turn = 1; max_mana = 10 };
  }

let game = new_game hearthstone_rules

(* let drawer = Card.make Library.card_drawer player1.player_id
   let novice1 = Card.make Library.novice player1.player_id
   let elemental_watcher1 = Card.make Library.elemental_watcher player1.player_id
   let emer_reaver = Card.make Library.emerald_reaver player1.player_id
   let arcane_int = Card.make Library.arcane_intelect player1.player_id
   let auctioneer1 = Card.make Library.auctioneer player1.player_id
   let auctioneer2 = Card.make Library.auctioneer player2.player_id
   let elem_watcher = Card.make Library.elemental_watcher player1.player_id
   let fireball = Card.make Library.fireball player1.player_id *)
let game = add_card_to_deck 0 Library.card_drawer game
let game = add_card_to_hand 0 Library.novice game
let game = add_card_to_hand 1 Library.novice game

let () =
  let _ = print_endline @@ string_of_gamestate game in
  (* let game1 = play_card 0 0 game in
     let _ = print_endline @@ string_of_gamestate game1 in *)
  ()
(*
  let game2 = lazy (play_card 0 0 (end_turn 1 (end_turn 0 game1))) in
  print_endline @@ string_of_gamestate (Lazy.force game2)
*)
