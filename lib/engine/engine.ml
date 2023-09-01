open BaseTypes
open Action

let string_of_gamestate (game : gamestate) : string =
  Utils.mod_string (Colour Magenta)
    ("Current player: " ^ string_of_int game.current_player)
  ^ "\n"
  ^ Player.string_of_players game.players

let get_player_triggers (player : playerstate) =
  List.fold_left ( @ ) []
    (List.map
       (fun (x : card_instance) -> List.map (fun y -> (x, y)) !x.card.triggers)
       player.board)
  @ List.fold_left ( @ ) []
      (List.map
         (fun (x : card_instance) ->
           List.map (fun y -> (x, y)) !x.card.triggers)
         player.hand)

let get_all_triggers (state : gamestate) =
  List.fold_left (fun ls pl -> ls @ get_player_triggers pl) [] state.players

let process_event event state =
  let triggers = get_all_triggers state in
  List.fold_left
    (fun state trigger ->
      if (fst @@ snd trigger) event (fst trigger) then
        match snd @@ snd trigger with
        | Self f -> apply_action (f (Card (fst trigger))) state
        | x -> apply_action x state
      else state)
    state triggers

let draw_card (player_nr : int) (game : gamestate) : gamestate =
  let player = List.nth game.players player_nr in

  let new_deck = match player.deck with [] -> [] | _ :: t -> t in
  let new_hand =
    match player.deck with [] -> player.hand | h :: _ -> h :: player.hand
  in
  let state =
    {
      game with
      players =
        Utils.replace player_nr game.players
          { player with deck = new_deck; hand = new_hand };
    }
  in
  match player.deck with
  | [] -> state
  | _ ->
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
            mana = player.mana - !card.card.cost;
          };
    }
  in
  let event = CardPlayed card in
  process_event event new_game

let end_turn (player_number : int) (game : gamestate) : gamestate =
  match player_number with
  | x when x = game.current_player ->
      let event = TurnEnded player_number in
      let new_game = process_event event game in
      print_endline @@ "Player " ^ string_of_int player_number ^ " turn ended";
      { new_game with current_player = (player_number + 1) mod 2 }
  | _ -> game

let new_game (rules : Rules.t) : gamestate =
  let player1 = Player.make rules in
  let player2 = Player.make rules in
  let players = [ player1; player2 ] in
  { players; current_player = 0 }

let add_card_to_deck player_number card game =
  let player = List.nth game.players player_number in
  let new_deck = Card.make card player_number :: player.deck in
  let new_player = { player with deck = new_deck } in
  let new_players = Utils.replace player_number game.players new_player in
  { game with players = new_players }

let add_card_to_hand player_number card game =
  let player = List.nth game.players player_number in
  let new_hand = Card.make card player_number :: player.hand in
  let new_player = { player with hand = new_hand } in
  let new_players = Utils.replace player_number game.players new_player in
  { game with players = new_players }
