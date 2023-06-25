open BaseTypes

let battlecry ev (c : card_instance) : bool =
  match ev with CardPlayed card -> card.card_id == c.card_id | _ -> false

let spell_cast ev _ : bool =
  match ev with CardPlayed card -> card.card.card_type = Spell | _ -> false

let card_type_played str ev _ : bool = 
  match ev with CardPlayed card -> List.exists (fun x -> x = str) card.card.tribes | _  -> false

let you_spell_cast ev (c : card_instance) : bool =
  match ev with
  | CardPlayed card -> card.card.card_type = Spell && card.owner = c.owner
  | _ -> false

let card_drawn ev (c : card_instance) : bool =
  match ev with CardDrawn card -> card.owner = c.owner | _ -> false
