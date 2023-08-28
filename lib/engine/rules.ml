open BaseTypes

type mana_rules = { max_mana : int; starting_mana : int; mana_per_turn : int }
type hp_rules = { starting_hp : int; hp_per_turn : int }

type card_draw_rules = {
  starting_hand_size : int;
  max_hand_size : int;
  (* draw_event : event; *)
  amount_drawn : int;
}

type t = {
  card_draw_rules : card_draw_rules;
  hp_rules : hp_rules option;
  mana_rules : mana_rules option;
}
