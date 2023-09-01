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
let game = add_card_to_deck 0 Library.card_drawer game
let game = add_card_to_deck 0 Library.novice game
let game = add_card_to_deck 0 Library.elemental_watcher game
let game = add_card_to_deck 0 Library.emerald_reaver game
let game = add_card_to_deck 0 Library.arcane_intelect game
let game = add_card_to_deck 0 Library.auctioneer game
let game = add_card_to_deck 0 Library.elemental_watcher game
let game = add_card_to_deck 0 Library.fireball game
let game = add_card_to_deck 1 Library.auctioneer game
let game = add_card_to_deck 1 Library.novice game
let game = add_card_to_deck 1 Library.novice game
let game = add_card_to_deck 1 Library.novice game

let rec draw_starting_hand player_id game =
  if
    List.length (List.nth game.players player_id).hand
    < hearthstone_rules.card_draw_rules.starting_hand_size
  then draw_card player_id game |> draw_starting_hand player_id
  else game

let () =
  let _ = print_endline @@ string_of_gamestate game in
  let game = draw_starting_hand 0 game in
  let game = draw_starting_hand 1 game in
  let _ = print_endline @@ string_of_gamestate game in
  let game = draw_card 1 game in
  let game = draw_card 1 game in
  let _ = print_endline @@ string_of_gamestate game in
  ()

(* let game1 = play_card 0 0 game in
   let _ = print_endline @@ string_of_gamestate game1 in *)

(*
  let game2 = lazy (play_card 0 0 (end_turn 1 (end_turn 0 game1))) in
  print_endline @@ string_of_gamestate (Lazy.force game2)
*)
