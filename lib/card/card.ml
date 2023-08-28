open BaseTypes
open Utils

let string_of_card (card : card_instance) : string =
  !card.card.name ^ " - Cost: "
  ^ string_of_int !card.card.cost
  ^ " Stats: "
  ^ string_of_int !card.card.attack
  ^ "/"
  ^ string_of_int !card.card_hp
  (* ^ " ID: "
  ^ string_of_int !card.card_id
  ^ " Owner: " ^ string_of_int !card.owner *)

let string_of_cardlist (cards : card_instance list) : string =
  List.fold_left
    (fun left right -> left ^ right)
    ""
    (List.map (fun card -> "\n" ^ string_of_card card) cards)

let make (card : card) (owner : int) : card_instance =
  let card_id = make_new_number () in
  ref
    {
      card;
      owner;
      card_id;
      card_counters = Counter.empty;
      card_hp = card.health;
    }
