let string_of_playerstate (player : BaseTypes.playerstate) : string =
  "Player : \n" ^ "HP: " ^ string_of_int player.hp ^ "\n" ^ "Hand: "
  ^ Card.string_of_cardlist player.hand
  ^ "\n\n" ^ "Deck: "
  ^ Card.string_of_cardlist player.deck
  ^ "\n\n" ^ "Board: "
  ^ Card.string_of_cardlist player.board
  ^ "\n\n"

let string_of_players (players : BaseTypes.playerstate list) : string =
  List.fold_left
    (fun x y -> x ^ "\n" ^ y)
    ""
    (List.map string_of_playerstate players)
