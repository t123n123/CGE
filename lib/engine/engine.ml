open BaseTypes
open Action

let string_of_gamestate (game : gamestate) : string =
  "Current player: "
  ^ string_of_int game.current_player
  ^ "\n"
  ^ Player.string_of_players game.players

let get_player_triggers (player : playerstate) =
  List.fold_left ( @ ) []
    (List.map
       (fun (x : card_instance) -> List.map (fun y -> (x, y)) (!x).card.triggers)
       player.board)
  @ List.fold_left ( @ ) []
      (List.map
         (fun (x : card_instance) -> List.map (fun y -> (x, y)) (!x).card.triggers)
         player.hand)

let get_all_triggers (state : gamestate) =
  List.fold_left (fun ls pl -> ls @ get_player_triggers pl) [] state.players

let process_event event state =
  let triggers = get_all_triggers state in
  List.fold_left
    (fun state trigger ->
      if (fst @@ snd trigger) event (fst trigger) then
        match (snd @@ snd trigger) with 
          | Self f -> apply_action (f (Card(fst trigger))) state
          | x -> apply_action x state 
      else state)
    state triggers

let draw_card (player_nr : int) (players : gamestate) : gamestate =
  let player = List.nth players.players player_nr in
  let new_deck = List.tl player.deck in
  let new_hand = List.hd player.deck :: player.hand in
  let state =
    {
      players with
      players =
        Utils.replace player_nr players.players
          { player with deck = new_deck; hand = new_hand };
    }
  in
  let event = CardDrawn (List.hd player.deck) in
  process_event event state

let play_card (player_nr : int) (card_nr : int) (game : gamestate) : gamestate =
  let player = List.nth game.players player_nr in
  let card = List.nth player.hand card_nr in
  let new_hand = Utils.remove card_nr player.hand in
  let new_board = card :: player.board in
  let new_game =
    {
      game with
      players =
        Utils.replace player_nr game.players
          {
            player with
            hand = new_hand;
            board = new_board;
            mana = player.mana - (!card).card.cost;
          };
    }
  in
  let event = CardPlayed card in
  process_event event new_game

let end_turn (player_number : int) (game : gamestate) : gamestate =
  match player_number with
  | x when x = game.current_player ->
      let event = EndTurn player_number in 
      let new_game = process_event event game in
      print_endline @@ "Player " ^ string_of_int player_number ^ " turn ended";
      { new_game with current_player = (player_number + 1) mod 2 }
  | _ -> game



