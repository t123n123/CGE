module Counter = Map.Make(String)

type card = {
  name : string;
  health : int;
  attack : int;
  cost : int;
  card_type : CardType.t;
  tribes : string list;
  triggers : trigger list;
}

and card_data = { card : card; mutable owner : int; card_id : int ; mutable card_counters : int Counter.t; card_hp : int; }
and card_instance = card_data ref

and event_checker = (event -> card_instance -> bool)
and trigger = event_checker * action

and event =
  | CardPlayed of card_instance
  | CardDeath of card_instance
  | CardDrawn of card_instance
  | EndTurn of int

and target = Card of card_instance | Player of int


and action =
  | Instant of (gamestate -> gamestate)
  | Self of (target -> action)
  | Targetted of (target -> action)

and playerstate = {
  hp : int;
  mana : int;
  max_mana : int;
  player_id : int;
  deck : card_instance list;
  hand : card_instance list;
  board : card_instance list;
}
(* Maybe make a map for each "card zone" for each player*)
and gamestate = { players : playerstate list; current_player : int }
