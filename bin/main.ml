open CGE.BaseTypes
open CGE.Engine
open CGE.Action
open CGE.Event
open CGE 

let drawer = Card.make Library.card_drawer 0
let novice1 = Card.make Library.novice 0
let elemental_watcher1 = Card.make Library.elemental_watcher 0
let emer_reaver = Card.make Library.emerald_reaver 0
let arcane_int = Card.make Library.arcane_intelect 0
let auctioneer1 = Card.make Library.auctioneer 0
let auctioneer2 = Card.make Library.auctioneer 1
let elem_watcher = Card.make Library.elemental_watcher 0

let game =
  {
    players =
      [
        {
          player_id = 0;
          hp = 30;
          mana = 0;
          max_mana = 0;
          deck = [arcane_int];
          hand = [emer_reaver; elem_watcher];
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
