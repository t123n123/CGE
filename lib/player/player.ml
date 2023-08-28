open BaseTypes

let string_of_playerstate (player : playerstate) : string =
  Utils.mod_string (Colour Yellow) ("Player : " ^ string_of_int player.player_id)
  ^ "\n"
  ^ Utils.mod_string (Colour Red) ("HP: " ^ string_of_int player.hp)
  ^ "\n"
  ^ Utils.mod_string (Colour Blue) ("Mana: " ^ string_of_int player.mana)
  ^ "\n\n" ^ "Hand: "
  ^ Card.string_of_cardlist player.hand
  ^ "\n\n" ^ "Deck: "
  ^ Card.string_of_cardlist player.deck
  ^ "\n\n" ^ "Board: "
  ^ Card.string_of_cardlist player.board
  ^ "\n\n"

let string_of_players (players : playerstate list) : string =
  List.fold_left
    (fun x y -> x ^ "\n" ^ y)
    ""
    (List.map string_of_playerstate players)

let make (rules : Rules.t) : playerstate =
  {
    hp = (match rules.hp_rules with Some r -> r.starting_hp | None -> 0);
    player_id = Utils.make_new_number ();
    mana = (match rules.mana_rules with Some r -> r.starting_mana | None -> 0);
    hand = [];
    deck = [];
    board = [];
    max_mana = (match rules.mana_rules with Some r -> r.max_mana | None -> 0);
  }
