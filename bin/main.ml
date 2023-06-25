open CGE.BaseTypes
open CGE.Engine
open CGE.Action
open CGE.Event
open CGE 
open Utils

let player1 = make_new_number ()
let player2 = make_new_number ()

let drawer = Card.make Library.card_drawer player1
let novice1 = Card.make Library.novice player1
let elemental_watcher1 = Card.make Library.elemental_watcher player1
let emer_reaver = Card.make Library.emerald_reaver player1
let arcane_int = Card.make Library.arcane_intelect player1
let auctioneer1 = Card.make Library.auctioneer player1
let auctioneer2 = Card.make Library.auctioneer player2
let elem_watcher = Card.make Library.elemental_watcher player1
let fireball = Card.make Library.fireball player1

let game =
  {
    players =
      [
        {
          player_id = player1;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [arcane_int];
          hand = [fireball; elem_watcher];
          board = [];
        };
        {
          player_id = player2;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [ ];
          hand = [ ];
          board = [auctioneer2];
        };
      ];
    current_player = 0;
  }

let () =
  let _ = print_endline @@ string_of_gamestate game in
  let game1 = (play_card 0 0 game) in
  let _ = print_endline @@ string_of_gamestate (game1) in 
  () (*
  let game2 = lazy (play_card 0 0 (end_turn 1 (end_turn 0 game1))) in
  print_endline @@ string_of_gamestate (Lazy.force game2)
*)