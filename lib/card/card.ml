let string_of_card (card : BaseTypes.card_instance) : string =
  (!card).card.name ^ " - Cost: "
  ^ string_of_int (!card).card.cost
  ^ " Stats: "
  ^ string_of_int (!card).card.attack
  ^ "/"
  ^ string_of_int (!card).card.health

let string_of_cardlist (cards : BaseTypes.card_instance list) : string =
  List.fold_left
    (fun left right -> left ^ right)
    ""
    (List.map (fun card -> "\n" ^ string_of_card card) cards)
